package MasonX::WebApp;

use strict;

use vars qw($VERSION);

$VERSION = 0.06;

use Exception::Class
    ( 'MasonX::WebApp::Exception' =>
      { alias => 'error',
        description => 'Generic super-class for MasonX::WebApp exceptions' },

      'MasonX::WebApp::Exception::Declaration' =>
      { isa   => 'MasonX::WebApp::Exception',
        alias => 'declaration_error',
        description => 'Attempted to use a feature without declaring something needed for it' },

      'MasonX::WebApp::Exception::Params' =>
      { isa   => 'MasonX::WebApp::Exception',
        alias => 'param_error',
        description => 'Bad parameters given to a method/function' },

      'MasonX::WebApp::Exception::Redirect' =>
      { isa   => 'MasonX::WebApp::Exception',
        alias => 'redirect_exception',
        description => 'Web app code generated redirect' },
    );

MasonX::WebApp::Exception->Trace(1);

use base 'Class::Data::Inheritable';

use Apache::Constants ();

use Class::Factory::Util;
use HTML::Mason::Interp;
use URI;

use Params::Validate
    qw( validate validate_pos validate_with UNDEF SCALAR BOOLEAN HASHREF OBJECT );
Params::Validate::validation_options
    ( on_fail => sub { param_error( join '', @_ ) } );


BEGIN
{
    __PACKAGE__->mk_classdata( 'ActionURIPrefix' );
    __PACKAGE__->mk_classdata( '_ActionURIPrefixRegex' );

    __PACKAGE__->mk_classdata( 'ApacheHandlerParams' );

    __PACKAGE__->mk_classdata( 'MasonGlobalName' );

    __PACKAGE__->mk_classdata( 'RequireRedirectAfterAction' );

    __PACKAGE__->mk_classdata( 'SessionWrapperParams' );
    __PACKAGE__->mk_classdata( 'UseSession' );

    __PACKAGE__->ActionURIPrefix('/submit/');
    __PACKAGE__->MasonGlobalName('$WebApp');
    __PACKAGE__->UseSession(0);
    __PACKAGE__->RequireRedirectAfterAction(1);
}

sub UseSession
{
    my $class = shift;

    if (@_)
    {
        my ($bool) = validate_pos( @_, { type => BOOLEAN } );

        $class->_UseSession_accessor($bool);

        if ( $class->_UseSession_accessor )
        {
            require Apache::Session::Wrapper;
        }
    }

    return $class->_UseSession_accessor;
}

sub SessionWrapperParams
{
    my $class = shift;

    if (@_)
    {
        my ($p) = validate_pos( @_, { type => HASHREF } );

        $class->_SessionWrapperParams_accessor($p);

        $class->UseSession(1);
    }

    return $class->_SessionWrapperParams_accessor;
}

sub ApacheHandlerParams
{
    my $class = shift;

    if (@_)
    {
        my ($p) = validate_pos( @_, { type => HASHREF } );

        $class->_ApacheHandlerParams_accessor($p);
    }

    return $class->_ApacheHandlerParams_accessor;
}

sub ActionURIPrefix
{
    my $class = shift;

    if (@_)
    {
        my ($prefix) = validate_pos( @_, { regex => qr{^(?:/|/.+/)$} } );

        $class->_ActionURIPrefix_accessor($prefix);

        $class->_ActionURIPrefixRegex( qr/^\Q$prefix\E/ );
    }

    return $class->_ActionURIPrefix_accessor;
}

sub _LoadActions
{
    my $class = shift;

    foreach my $sub ( $class->subclasses )
    {
        eval "use ${class}::$sub";
        die $@ if $@;
    }
}

sub new
{
    my $class = shift;

    my %p = validate_with( params => \@_,
                           spec   => { apache_req => { isa  => 'Apache' },
                                       args       => { type => HASHREF },
                                     },
                           allow_extra => 1,
                         );

    my $self =  bless { __apache_req__ => delete $p{apache_req},
                        __args__       => delete $p{args},
                      }, $class;

    $self->__set_wrapper if $self->UseSession;

    eval
    {
        $self->_init(%p) if $self->can('_init');

        $self->_handle_action;
    };

    if ($@)
    {
        if ( UNIVERSAL::isa( $@, 'MasonX::WebApp::Exception::Redirect' ) )
        {
            # This shouldn't propogate out to the caller
            undef $@;
        }
        else
        {
            UNIVERSAL::can( $@, 'rethrow' ) ? $@->rethrow : die $@;
        }
    }

    return $self;
}

sub apache_req { $_[0]->{__apache_req__} }
sub args       { $_[0]->{__args__} }

sub __set_wrapper
{
    my $self = shift;

    $self->{__wrapper__} = $self->_make_session_wrapper;
}

sub _make_session_wrapper
{
    return Apache::Session::Wrapper->new( %{ $_[0]->SessionWrapperParams } );
}

sub session_wrapper
{
    error "Cannot call session_wrapper() method unless UseSession is true"
        unless $_[0]->UseSession;

    return $_[0]->{__wrapper__};
}

sub session { $_[0]->session_wrapper->session }

sub _handle_action
{
    my $self = shift;

    return if $self->redirected;

    my $prefix_re = $self->_ActionURIPrefixRegex;
    my ($action) = $self->apache_req->uri =~ m{$prefix_re(\w+)};

    return unless defined $action && length $action;

    param_error "Invalid action: $action" unless $self->can($action);

    $self->$action();

    # This code is unlikely to be executed, as issuing a redirect
    # causes an exception
    error "No redirect was issued after the $action action."
        unless $self->redirected || ! $self->RequireRedirectAfterAction;
}

sub redirect
{
    my $self = shift;
    my %p = @_;

    my $uri = exists $p{uri} ? $p{uri} : $self->uri( %p, xhtml => 0 );

    if ( my $m = HTML::Mason::Request->instance )
    {
        $m->redirect($uri);
    }
    else
    {
        $self->{__redirected__} = 1;

        my $r = $self->apache_req;

        $r->method('GET');
        $r->headers_in->unset('Content-length');
        $r->err_header_out( Location => $uri );
        $r->status( Apache::Constants::REDIRECT() );

        $r->send_http_header;

        redirect_exception();
    }
}

sub redirected { $_[0]->{__redirected__} }

sub uri
{
    shift;
    my %p = validate( @_,
                      { scheme   => { type => SCALAR, default  => 'http' },
                        username => { type => SCALAR, optional => 1 },
                        password => { type => SCALAR, default  => '' },
                        host     => { type => SCALAR, optional => 1 },
                        port     => { type => SCALAR, optional => 1 },
                        path     => { type => SCALAR },
                        query    => { type => HASHREF, default => {} },
                        fragment => { type => SCALAR,  optional => 1 },
                        xhtml    => { type => BOOLEAN, default => 1 },
                      },
                    );

    my $uri = URI->new;

    if ( defined $p{host} )
    {
        $uri->scheme( $p{scheme} );

        if ( defined $p{username} )
        {
            $uri->authority( "$p{username}:$p{password}" );
        }

        $uri->host( $p{host} );
        $uri->port( $p{port} ) if $p{port};
    }

    $uri->path( $p{path} );

    # $uri->query_form doesn't handle hash ref values properly
    while ( my ( $k, $v ) = each %{ $p{query} } )
    {
        $p{query}{$k} = UNIVERSAL::isa( $v, 'HASH' ) ? [ %$v ] : $v;
    }

    $uri->query_form( %{ $p{query} } ) if keys %{ $p{query} };

    $uri->fragment( $p{fragment} ) if $p{fragment} ;

    my $canonical = $uri->canonical;

    # make URI XHTML-compliant
    $canonical =~ s/&(?!amp;)/&amp;/g if $p{xhtml};

    # force stringification
    return $canonical . '';
}

sub _handle_error
{
    my $self = shift;

    my %p = validate_with( params => \@_,
                           spec   => { error     => { type => SCALAR|OBJECT },
                                       save_args => { type => HASHREF, default => {} },
                                     },
                           allow_extra => 1,
                    );

    if ( UNIVERSAL::can( $p{error}, 'messages' ) && $p{error}->messages )
    {
        $self->_add_error_message($_) for $p{error}->messages;
    }
    elsif ( UNIVERSAL::can( $p{error}, 'message' ) )
    {
        $self->_add_error_message( $p{error}->message );
    }
    else
    {
        # force stringification
        $self->_add_error_message( "$p{error}" );
    }

    while ( my ( $k, $v ) = each %{ $p{save_args} } )
    {
        $self->_save_arg( $k => $v );
    }

    delete @p{ 'error', 'save_args' };

    $self->redirect(%p);
}

sub _save_arg  { $_[0]->session->{__saved_args__}{ $_[1] } = $_[2] }

sub saved_args { $_[0]->session->{__saved_args__} || {} }

sub _add_message       { push @{ $_[0]->session->{__messages__} }, $_[1] }

sub _add_error_message { push @{ $_[0]->session->{__errors__} }, $_[1] }

sub messages { my $s = $_[0]->session;
               $s->{__messages__} ? @{ delete $s->{__messages__} } : () }

sub errors   { my $s = $_[0]->session;
               $s->{__errors__} ? @{ delete $s->{__errors__} } : () }

sub clean_session { delete @{ $_[0]->session }{ qw( __messages__ __errors__ __saved_args__ ) } }

sub handler ($$)
{
    my $class = shift;
    my $r     = shift;

    my $ah = $class->_apache_handler_object;

    my $args = $ah->request_args($r);

    my $app = $class->new( $r, $args );

    return Apache::Constants::REDIRECT() if $app->redirected;

    if ( $ah->interp->compiler->can('add_allowed_globals')
         && defined $class->MasonGlobalName )
    {
        $ah->interp->compiler->add_allowed_globals( $class->MasonGlobalName );
        $ah->interp->set_global( '$App' => $class->MasonGlobalName );
    }

    my $return = eval { $ah->handle_request($r) };

    my $err = $@;

    # We want to wipe out the variable before the request ends,
    # because if the $ah variable persists, then so does the interp,
    # which means the $app object won't be destroyed until the next
    # request in this process, which can hose up sessions big time.
    $ah->interp->set_global( '$App' => undef );

    $app->clean_session if $class->UseSession;

    die $err if $err;

    return $return;
}

sub _apache_handler_object
{
    my $class = shift;

    return MasonX::WebApp::ApacheHandler->new( %{ $class->ApacheHandlerParams } );
}


package MasonX::WebApp::ApacheHandler;

use base 'HTML::Mason::ApacheHandler';

sub request_args
{
    my $self = shift;
    my $r = shift;

    return $r->pnotes('__request_args__') if $r->pnotes('__request_args__');

    my $args = ($self->SUPER::request_args($r))[0] || {};

    $r->pnotes( __request_args__ => $args );

    return $args;
}


1;

__END__

=head1 NAME

MasonX::WebApp - Works with Mason to do processing before Mason is invoked

=head1 SYNOPSIS

  # Create a subclass of MasonX::WebApp
  package My::WebApp;

  use base 'MasonX::WebApp';

  sub _init
  {
      # do neat stuff
  }

  # In your Apache config file

  <Location />
    SetHandler   perl-script
    PerlHandler  My::WebApp
  </Location>

=head1 DESCRIPTION

C<MasonX::WebApp> works with Mason to let you do processing I<before>
Mason is ever invoked.  There are a number of things that one might
want to do:

=over 4

=item * Argument munging

You might want to make sure all incoming arguments are UTF-8 encoded.
Or you might want to create some objects which Mason will see as
incoming arguments.  For example, a "user_id" parameter could be
turned into a user object.

=item * Handle requests without Mason

If you're not generating output for the browser other than a redirect,
then there's no reason to use Mason.  You can use a C<MasonX::WebApp>
subclass to handle all form submissions, for example.

This has the added benefit of making it easier to preload this code
once during server startup.

=item * Authorization checks

Why do authorization checks in Mason if a failed check just leads to a
redirect or NOT FOUND return code?

=back

=head1 USAGE

To use C<MasonX::WebApp>, you should create a C<MasonX::WebApp> subclass.
By itself, C<MasonX::WebApp> won't do a whole lot for you, but it
provides a nice framework for building on.

=head2 What MasonX::WebApp Provides

C<MasonX::WebApp>, out of the box, provides the following:

=over 4

=item * Session creation

You can declare your session parameters, and C<MasonX::WebApp> will
create an C<Apache::Session::Wrapper> object for you, available via
the C<session()> method.  Alternately, you can implement your own
session creation method in your subclass.

=item * Argument munging

The arguments which will eventually be passed to Mason are available
via the C<args()> method.  This method returns a hashref, and any
changes made to this reference will affect the arguments eventually
passed to Mason.

=item * "Actions"

C<MasonX::WebApp> will call appropriate methods based on the URI.
These methods are determined by removing a prefix from the URI
(settable via a class method), and then using the remainder as a
method name to be called on the webapp object.

=item * Messages, errors, and "saved arguments"

If you are using sessions, the webapp object provides methods to store
regular messages, error messages, and save arguments (to re-populate a
form, for example) in the session.  It also provides methods to
retrieve these.

=item * Convenient uri creation

The C<uri()> method provides a nice flexible API for creating URIs.

=back

You can set some parameters for your subclass declaratively, by
calling class methods.  These methods store data using
C<Class::Data::Inheritable>, so you can inherit from your subclasses
and inherit these parameters.

=head2 Declarative Parameters

The following class methods are offered for declaring parameters:

=over 4

=item * ActionURIPrefix

This is the prefix used to determine which, if any, "action" method
should be called on the webapp object.  By default, this is
F</submit/>.  So if a request comes in for F</submit/login>, then the
C<login()> method will be called.

If you change this, your prefix must also start and with a slash (/).

=item * ApacheHandlerParams

This should be a hash reference of options that will be passed to the
C<MasonX::WebApp::ApacheHandler> class's C<new()> method when creating
a new ApacheHandler object.  You don't need to set this if you are
creating the ApacheHandler from scratch in your subclass, and/or if
you are providing your own mod_perl C<handler()> subroutine/method.

The default C<handler()> will create a new
C<MasonX::WebApp::ApacheHandler> object on every request, using these
parameters.

=item * MasonGlobalName

The variable name to use for the webapp object in Mason components.
The default C<handler()> sets this global.

The default value for this is C<$WebApp>.

=item * SessionWrapperParams

A hash reference of parameters to be passed to the
C<Apache::Session::Wrapper> class's C<new()> method.

You don't need to set this if you are creating your own session
wrapper object.

Setting this also causes C<UseSession> to be set to a true value.

=item * UseSession

Set this to true if you are creating your own session wrapper object,
so that C<MasonX::WebApp> knows it can call C<session()> internally.

=back

=head2 Exceptions

Some methods throw exceptions.  Exceptions classes are created using
C<Exception::Class>.

=head2 Public Methods

The folowing methods are public, and can be called from subclasses or
from elsewhere, like in Mason components.

=over 4

=item * new()

This is the constructor method.  It expects to receive at least two
arguments:

=over 8

=item * apache_req

An Apache request object, which must be an object in the C<Apache>
class, or a subclass (like C<Apache::Request> or C<Apache::Filter>).

=item * args

A hash reference of arguments.  If you are using the
C<MasonX::WebApp::ApacheHandler> class, you can use the return value
of its C<request_args()> method.

=back

The new method will do the following:

Call C<_set_session()> if C<UseSession()> is true.

Call C<_init()>, if you have an C<_init()> method defined in your
subclass.  If additional arguments are given then they will be passed
along to your C<_init()> method, if you have one.  The call to
C<_init()> is wrapped in an eval block.  If an exception is throws,
and that exception is not a C<MasonX::WebApp::Exception::Redirect>
exception, then it will be rethrown.  Redirect exceptions are I<not>
rethrown.

Call C<_handle_action()>.

Return the newly created webapp object.

=item * apache_req

Returns the Apache request given to the C<new()> method.

=item * args

Returns a hash reference containing the arguments passed to the
C<new()> method.  Since this is the same reference as is stored in the
C<MasonX::WebApp::ApacheHandler> object, any changes to this reference
will be visible to Mason components.

=item * session_wrapper

Returns the C<Apache::Session::Wrapper> object for the webapp object.

If C<UseSession()> is not true, calling this method throws an
exception.

=item * session

A shortcut for calling C<< $webapp->session_wrapper->session >>.

If C<UseSession()> is not true, calling this method throws an
exception.

=item * redirect

This method can take a number of named parameters.  If it is given a
"uri" parameter, then it uses this URI for the redirection.
Otherwise, it takes any parameters it is given and calls the C<uri()>
method with them.  When it calls C<uri()>, it sets the "xhtml"
parameter to false, so you do not need to do this.

If called inside the context of a Mason request, it calls
C<redirect()> on the Mason request object.

Otherwise it sets the value of C<redirected()> to true, sends a
redirect using the apache request object, and then throws a
C<MasonX::WebApp::Exception::Redirect> exception.

=item * redirected

Returns a boolean value indicating whether or not C<redirect()> has
been called on the webapp object.

=item * uri

This creates a URI string based on the parameters it receives.  It
accepts the following parameters:

=over 8

=item * path

The path portion of the URI.  This is the only required parameter.

=item * query

A hash reference which will be turned into a query string.  The keys
of the hash reference may point to scalars, array references, or hash
references.  Hash reference values are treated the same way as array
references.

=item * fragment

Optional

=item * host

Optional.  By default, URIs are relative, and this is not used.

=item * port

Optional.  This is ignored unless "host" is also passed.

=item * scheme

Defaults to "http", but since URIs are relative by default, this is
ignored unless "host" is also passed.

=item * username

=item * password

Optional.  These are both ignored unless "host" is also passed.  If
"password" is passed without a "username", it is ignored.

=item * xhtml

Defaults to true.  If this is true, then the returned URI will have
any ampersands (&) in the query string HTML-escaped (&amp;).

=back

=item * messages

Returns an array of non-error messages stored in the session.  This
method is I<destructive>, as calling it removes the messages from the
session.

If you are not using sessions, calling this method throws an
exception.


=item * errors

Returns an array of error messages stored in the session.  This method
is I<destructive>, as calling it removes the error messages from the
session.

If you are not using sessions, calling this method throws an
exception.

=item * saved_args

Returns a hash reference of arguments saved in the session.  This
method is I<not> destructive.  If you are saving arguments in the
session, you should probably make sure that C<clean_session()> is
called at the end of every request.  The default C<handler()> sub does
this.

If you are not using sessions, calling this method throws an
exception.

=item * clean_session

Removes any messages, error messages, and saved args stored in the
session.  This should be called a the end of each request in order to
prevent these value leaking over into the next request.

If you are not using sessions, calling this method throws an
exception.

=back

=head2 Protected Methods

These methods are intended to be called, and/or overridden by your
subclass.

=over 4

=item * _LoadActions

If you want to define actions in other files, like
C<My::WebApp::User>, this method provides a handy way to load all of
them at once.  It looks for modules under your subclass's package name
and loads them.  So if your subclass is in the package
C<Foo::Bar::WebApp>, then it looks for modules matching
C<Foo::Bar::WebApp::*>.

=item * _make_session_wrapper

This method is called during object construction if C<UseSession> is
true.  By default, it creates a new C<Apache::Session::Wrapper> object
with the parameters from C<SessionWrapperParams>.  You can override
this method to provide your own session wrapper creation.

=item * _handle_action

This method is called during object construction.  If a redirect was
done earlier in the object creation process, then it does nothing.
Otherwise, it looks at the requested URI to see if it matches the
C<ActionURIPrefix>.  If it does, it turns the URI into a method name
by stripping off the prefix, and it calls that method on the webapp
object.

You can override this to provide your own dispatching system for
requests.

Note that this method should I<not> call out to Mason.  It should only
be used for actions that don't need Mason.

=item * _save_arg

Given a key and value, this method saves them in the session so that
they will be available via the C<saved_args()> method.

If C<UseSession()> is not true, calling this method throws an
exception.

=item * _add_message

Given a string, this method stores that string in the session so that
it is available via the C<messages()> method.

If C<UseSession()> is not true, calling this method throws an
exception.

=item * _add_error_message

Given a string, this method stores that string in the session so that
it is available via the C<errors()> method.

If C<UseSession()> is not true, calling this method throws an
exception.

=item * _handle_error

This method can be used to handle exceptions that occur during
actions.

It provides a quick way to store error messages and arguments in the
session, and then issue a redirect.

It takes several parameters:

=over 8

=item * error

This should be either a scalar or an object.  If it is a scalar, this
is assumed to be a simple error message.

If an object is given, then it first looks for a C<messages()> method
in that object.  This method should return an array of scalars, each
of which represents an error message.

Otherwise it looks for a method called C<message()>, which should
return a single scalar.

It adds each error message to the session via the
C<_add_error_message()> method.

=item * save_args

This is a hash reference of arguments that should be saved in the
session.  Each key/value pair will be saved by calling the the
C<_save_arg()> method.

=back

All other arguments are passed along to the C<redirect()> method.

If C<UseSession()> is not true, calling this method throws an
exception.

=item * _apache_handler_object

This method is called in the default C<handler()> method in order to
create a new C<MasonX::WebApp::ApacheHandler> object.  It simply calls
that class's C<new()> method with the parameters set via
C<ApacheHandlerParams>.

=back

=head2 Hash Keys in the WebApp and Session Objects

In order to avoid stepping on your toes, all hash keys in the webapp
object, and all keys that it creates in the session object, are of the
form "__blahblah__".  In other words, they always start and end with
two underscores (__).  This should make it easy to avoid name
conflicts when subclassing this module or when using the session it
provides.

=head2 The Default handler() Method

The C<MasonX::WebApp> class provides a default handler method.  This
is the code:

  sub handler ($$)
  {
      my $class = shift;
      my $r     = shift;

      my $ah = $class->_apache_handler_object;

      my $args = $ah->request_args($r);

      my $app = $class->new( $r, $args );

      return Apache::Constants::REDIRECT() if $app->redirected;

      if ( $ah->interp->compiler->can('add_allowed_globals')
           && defined $class->MasonGlobalName )
      {
          $ah->interp->compiler->add_allowed_globals( $class->MasonGlobalName );
          $ah->interp->set_global( '$App' => $class->MasonGlobalName );
      }

      my $return = eval { $ah->handle_request($r) };

      my $err = $@;

      # We want to wipe out the variable before the request ends,
      # because if the $ah variable persists, then so does the interp,
      # which means the $app object won't be destroyed until the next
      # request in this process, which can hose up sessions big time.
      $ah->interp->set_global( '$App' => undef );

      $app->clean_session if $class->UseSession;

      die $err if $err;

      return $return;
  }

I would recommend that instead of using this, you create your own
mod_perl handler that does something similar, because this one is not
very efficient, given that it creates a new
C<MasonX::WebApp::ApacheHandler> object for each request.  It is
provided primarily as a reference implementation, and so that others
can experiment with this webapp code quickly.

In your own handler, there are several important guidelines you should
follow.

=over 4

=item

First of all, your should use the C<MasonX::WebApp::ApacheHandler>
class.  This is a subclass of Mason's ApacheHandler class that caches
the value of C<request_args()>.  This is done so that these arguments
can be passed to the C<MasonX::WebApp> constructor and still be made
available to Mason.  It also makes sure that Mason's arguments are the
I<same> hash reference as is available from the C<args()> method.
This is very important if you want to do any argument munging in your
subclass.  Also, since mod_perl will only read POSTed data once,
without this caching Mason would not see any arguments at all!

=item

After creating a new webapp object, make sure to check the value of
the C<redirected()> method for that object.  If it is true, you should
return the C<REDIRECT> constant from Apache::Constants.

=item

If you are using the message, error message, or saved arg features,
you should make sure that C<clean_session()> is called at the end of
every request.  This means that you need to wrap the call to the
ApacheHandler's C<handle_request()> method in an eval block.

=item

If you use the C<set_global()> method to make the webapp object
available to your components, B<and> your ApacheHandler objects
persist across requests, then you need to call C<set_global()> again
after the request is handled, and this time set that global to undef.
This ensures that the webapp object will be destroyed.

A safer alternative, if you know what class your components will be
compiled in, is to do this:

 local $HTML::Mason::Commands::App = $app;

The use of C<local> ensures that $app will go out of scope at the end
of C<handler()> subroutine.

=back

You can, of course, do anything you want in your own C<handler()>
method.  I often create an C<Apache::Request> object with a "POST_MAX"
parameter, in order to prevent a DoS from a ridiculously large POST.

I also often handle errors without dying, and instead will log them
and present a more friendly page to the user.  If you want to do this,
keep in mind that constructing a webapp object can throw exceptions,
so you may want to trap these in an C<eval> block.

If you do something cool with this code, write about it on the Mason
HQ site, masonhq.com (which is a big wiki), or send a post to the
Mason users list.

=head1 BUGS

As of this writing, the most recent version of
Class::Data::Inheritable (0.02) has a bug which causes subroutine
redefined errors when C<MasonX::WebApp> is loaded.  These errors can
safely be ignored.

=head1 SEE ALSO

If you like the basic idea of this code (run things before a Mason
component is invoked), but you don't want to create a subclass, I
encourage you to take a look at David Wheeler's
C<MasonX::Interp::WithCallbacks> module.  In fact, I encourage you to
take a look at it anyway, since it may be more appropriate than this
one, depending on your needs.

=head1 SUPPORT

Bug reports and requests for help should be sent to the mason-users
list.  See http://www.masonhq.com/resources/mailing_lists.html for
more details.

=head1 AUTHOR

Dave Rolsky, <autarch@urth.org>

=head1 COPYRIGHT

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

The full text of the license can be found in the LICENSE file included
with this module.

=cut

use strict;

use Module::Build;

Module::Build->new
    ( module_name => 'MasonX::WebApp',
      license     => 'perl',
      requires    => { 'Apache::Session::Wrapper' => 0.17,
                       'Class::Data::Inheritable' => 0,
                       'Class::Factory::Util'     => 0,
                       'Exception::Class'         => 0,
                       'HTML::Mason'              => 1.10,
                       'URI'                      => 0,
                     },
      sign        => 1,
    )->create_build_script;

<?php

require('./wp-content/plugins/application-passwords/application-passwords.php');

Application_Passwords::create_new_application_password(1, "offset");

WP_CLI::success( "The script has run!" );
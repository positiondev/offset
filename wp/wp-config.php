<?php


// ** MySQL settings ** //
/** The name of the database for WordPress */
define('DB_NAME', 'offset_test');

/** MySQL database username */
define('DB_USER', 'offset');

/** MySQL database password */
define('DB_PASSWORD', '111');

/** MySQL hostname */
define('DB_HOST', 'localhost');

/** Database Charset to use in creating database tables. */
define('DB_CHARSET', 'utf8');

/** The Database Collate type. Don't change this if in doubt. */
define('DB_COLLATE', '');

define('AUTH_KEY',         '9eu%H*;p4XdLuLj5P0bUwf|J, j<+]Z$iZo7[p5+~fP1l,.t#2vM?nRgnHmjo^&V');
define('SECURE_AUTH_KEY',  '-xZJk6;fpHV7KzB)6;f%yY_enWsZBjcsPi>0!=[9g~/)YCXmbw1hDYS4VWsO(t]U');
define('LOGGED_IN_KEY',    ')v:{qqs5<#bJj9 d5wE3ZQPb w1T.DS%$8d0;&O#,uIe4pp+JnMzWal[sU)6f#+4');
define('NONCE_KEY',        'A?X|?r/;TB8CHi-X)7I^)l@f8u|r*N|x-wT-dN4x CMT~k ,lnS$T@Kw<Zz-_8`_');
define('AUTH_SALT',        'tQ4lrrZdC_l1d]?6}n0X+};)06R GV<qzjmnY*8aN_{0^QO5z| 9D0Yr NkC[-KJ');
define('SECURE_AUTH_SALT', 'np)K2ZB~BO.j6e%K>f8JZv(`keeR%sx&.z? qUW{ghOsJ3knqAXLuz&/q;[0(p2t');
define('LOGGED_IN_SALT',   ':$+M/1f0aR{9RK~Dh7bFB4}CdI>LE8Q*t2+ :4N-HBkW&w+vLzb|>FFY.RPfIz_?');
define('NONCE_SALT',       'myv&]Rm!A<|e},+FXwR_-!GV*cp5*X,NA`r=9Nks|C+0fs!ox:5<sp=f7M[wHIx_');


$table_prefix = 'wp_';





/* That's all, stop editing! Happy blogging. */

/** Absolute path to the WordPress directory. */
if ( !defined('ABSPATH') )
	define('ABSPATH', dirname(__FILE__) . '/');

/** Sets up WordPress vars and included files. */
require_once(ABSPATH . 'wp-settings.php');

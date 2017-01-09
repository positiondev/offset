<?php
/* @param string $meta Meta name.
 * @param array $details Contains the details for the field.
 * @param string $value Contains input value;
 * @param string $context Context where the function is used. Depending on it some actions are preformed.;
 * @return string $element input element html string. */

$element .= '<input type="text" name="'. $single_prefix . esc_attr( Wordpress_Creation_Kit::wck_generate_slug( $details['title'], $details ) ) .'" id="';
if( !empty( $frontend_prefix ) )
	$element .=	$frontend_prefix;
$element .= esc_attr( Wordpress_Creation_Kit::wck_generate_slug( $details['title'], $details ) ) .'" value="'. ( ! empty( $value ) ? esc_attr( $value ) : esc_attr( $details['default'] ) ) .'" data-default-color="'. ( ! empty( $details['default'] ) ? esc_attr( $details['default'] ) : '' ) .'" class="mb-colorpicker mb-field"/>';
$element .= '<script type="text/javascript">
	jQuery( document ).ready( function( $ ) {
		$( ".mb-colorpicker" ).iris( {} );

		$( document ).click( function( e ) {
			if( ! $( e.target ).is( ".mb-colorpicker, .iris-picker, .iris-picker-inner, .iris-slider, .iris-slider-offset, .iris-square-handle, .iris-square-value" ) ) {
				$( ".mb-colorpicker" ).iris( "hide" );
			}
		} );

		$( ".mb-colorpicker" ).click( function( event ) {
			$( ".mb-colorpicker" ).iris( "hide" );
			$( this ).iris( "show" );
		} );
	} );
</script>';
?>
<?php 
 /* @param string $meta Meta name.	 
 * @param array $details Contains the details for the field.	 
 * @param string $value Contains input value;
 * @param string $context Context where the function is used. Depending on it some actions are preformed.;
 * @return string $element input element html string. */

$random_id = "wck_wisi_editor_". time() . '-' . rand(); 
$element .= '<textarea name="'. $single_prefix . esc_attr( Wordpress_Creation_Kit::wck_generate_slug( $details['title'], $details  ) ) .'" class="mb-textarea mb-field '. esc_attr( Wordpress_Creation_Kit::wck_generate_slug( $details['title'], $details ) ) .'" id="'. $random_id .'">'. esc_html( $value ) .'</textarea>';
$element .= '<script type="text/javascript">jQuery( function(){	
		CKEDITOR.config.allowedContent = true;
		CKEDITOR.replace("'. $random_id .'");
	});</script>';
?>


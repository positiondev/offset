<?php
/* FrontEnd Posting for WordPress */

class WCK_FrontEnd_Posting extends Wordpress_Creation_Kit{

	private $defaults = array(							
							'form_title' => 'Form',
							'post_type' => 'post',
							'form_name' => '',
							'admin_approval' => 'no',
							'anonymous_posting' => 'no',							
							'assign_to_user' => 1,
							'meta_array' => array(),
							'taxonomies' => array(),
							'single_cfcs' => array()
						);
	private $args;
	
	/* this will hold the meta_arrays for any single CFCs */
	private $single_cfcs;
	
	
	static $wck_fep_add_scripts;	
	
	/* Constructor method for the class. */
	function __construct( $args ) {		
		
		/* Merge the input arguments and the defaults. */
		$this->args = wp_parse_args( $args, $this->defaults );	

		/* populate single_cfcs arg with the meta_array of any single CFCs that are added to the form. 
		the meta_name will be used as the key */
		if( function_exists( 'wck_cfc_create_boxes_args' ) ){
			if( !empty( $this->args['meta_array'] ) ){
				foreach ( $this->args['meta_array'] as $details ){
					$all_box_args = wck_cfc_create_boxes_args();
					/* add single CFCs */						
					if( !empty( $all_box_args ) ){
						foreach( $all_box_args as $box_args ){					
							if( ( $box_args['post_type'] == $this->args['post_type'] ) && ( $box_args['metabox_title'] == $details['title']  ) ){							
								/* treat single cfcs case */
								if( $box_args['single'] )
									$this->single_cfcs[$box_args['meta_name']] = $box_args['meta_array'];							
									$this->single_cfcs[$box_args['meta_name'].'_unserialize_fields'] = $box_args['unserialize_fields'];
							}
						}
					}
				}
			}
		}
		
		/* register the shortcode */
		add_shortcode( 'fep', array( &$this, 'wck_fep_form_in_frontend') );
		
		/*print scripts*/
		add_action( 'get_header', array( &$this, 'wck_fep_register_script' ), 12 );
		add_action( 'wp_footer', array( &$this, 'wck_fep_print_script' ) );
		add_action( 'wp_head', array( &$this, 'wck_fep_print_ajax_url' ) );
		add_action( 'wp_enqueue_scripts', array( &$this, 'wck_fep_enqueue_scripts' ) );
		
		// Set up the AJAX hooks
		add_action( "wp_ajax_wck_fep_add_post".$this->args['form_name'], array( &$this, 'wck_fep_add_post') );
		add_action( "wp_ajax_nopriv_wck_fep_add_post".$this->args['form_name'], array( &$this, 'wck_fep_add_post') );
		
		add_action("wp_ajax_wck_add_meta".$this->args['form_name'], array( &$this, 'wck_add_meta') );
		add_action("wp_ajax_nopriv_wck_add_meta".$this->args['form_name'], array( &$this, 'wck_add_meta') );
				
		add_action("wp_ajax_wck_fep_create_frontend_form_".$this->args['form_name'], array( &$this, 'wck_fep_create_frontend_form') );
		add_action("wp_ajax_nopriv_wck_fep_create_frontend_form_".$this->args['form_name'], array( &$this, 'wck_fep_create_frontend_form') );				
	}	
	
	/**
	 * The function used to register scripts used by FEP
	 *
	 * @since 1.0.0	
	 */
	static function wck_fep_register_script(){
		//datepicker
		wp_enqueue_script('jquery-ui-datepicker');		
		wp_enqueue_style('jquery-style', plugins_url( '', dirname(__FILE__) ).'/assets/datepicker/datepicker.css');

		//colorpicker
		wp_enqueue_style( 'wp-color-picker' );
		wp_enqueue_style( 'wck-colorpicker-style', plugins_url( '', dirname(__FILE__) ).'/assets/colorpicker/colorpicker.css', false, '1.0' );
		wp_enqueue_script( 'iris', admin_url( 'js/iris.min.js' ), array( 'jquery-ui-draggable', 'jquery-ui-slider', 'jquery-touch-punch' ), false, 1 );
		wp_enqueue_script( 'wp-color-picker', admin_url( 'js/color-picker.min.js' ), array( 'iris' ), false, 1 );

		//phone
		wp_enqueue_script( 'wck-jquery-inputmask', plugins_url( '', dirname(__FILE__) ).'/assets/phone/jquery.inputmask.bundle.min.js', array( 'jquery' ), false, 1 );

        // map
        $options = get_option( 'wck_extra_options' );

        if( !empty( $options[0]['google-maps-api'] ) ) {
            wp_enqueue_script( 'wck-google-maps-api-script', 'https://maps.googleapis.com/maps/api/js?key=' . $options[0]['google-maps-api'] . '&libraries=places', array('jquery') );
            wp_enqueue_script( 'wck-google-maps-script', plugin_dir_url( __FILE__ ) . '../assets/map/map.js', array('jquery') );
            wp_enqueue_style( 'wck-google-maps-style', plugin_dir_url( __FILE__ ) . '../assets/map/map.css' );
        }

		/* FEP script */		
		wp_register_script( 'wck-fep', plugins_url('wck-fep.js', __FILE__ ), array('jquery'), '1.0', true );
		wp_register_style( 'wck-fep-css', plugins_url('wck-fep.css', __FILE__ ) );
		/* register WCK script for frontend */		
		wp_register_script( 'wck-js', plugins_url( '', dirname(__FILE__) ).'/wordpress-creation-kit.js', array('jquery', 'jquery-ui-datepicker'), '1.0', true );
			
		// wysiwyg				
		wp_register_script( 'wck-ckeditor', plugins_url( '', dirname(__FILE__) ).'/assets/js/ckeditor/ckeditor.js', array(), '1.0', true );		
		
		/* media upload wck script */		
		wp_register_script('wck-upload-field', plugins_url('/../fields/upload.js', __FILE__), array('jquery') );
	}
	
	/**
	 * The function used to print scripts used by FEP in the footer of the page
	 *
	 * @since 1.0.0	
	 */
	static function wck_fep_print_script() {
		/* if the static var set by the shortcode isn't true do nothing */
		if ( ! self::$wck_fep_add_scripts )
			return;
 
		wp_print_scripts('wck-fep');
		wp_print_styles('wck-fep-css');
		wp_print_scripts('wck-js');
		
		wp_print_scripts( 'jquery-ui-draggable' );
		wp_print_scripts( 'jquery-ui-droppable' );
		wp_print_scripts( 'jquery-ui-sortable' );
		wp_print_styles( 'thickbox' );						
		wp_print_scripts( 'thickbox' );			
		
		// wysiwyg		
		wp_print_scripts( 'wck-ckeditor' );

        /* media upload wck script */
        wp_print_scripts( 'wck-upload-field' );
	}
	
	/**
	 * The function used to print ajax url used by FEP in the header of the page
	 *
	 * @since 1.0.0	
	 */
	static function wck_fep_print_ajax_url() {
		/* ajax url for frontend */
		echo '<script type="text/javascript">var wckAjaxurl = "'. admin_url('admin-ajax.php') .'";</script>';
	}
	
	/**
	 * The function to enqueue jquery in header of the theme
	 *
	 * @since 1.0.0	
	 */
	static function wck_fep_enqueue_scripts(){
		wp_enqueue_script( 'jquery' );
	}
	
	
	/**
	 * Shortcode Function
	 *
	 * @since 1.0.0	
	 *	 	 
	 * @param array $atts Shortcode attributes	
	 * @return string $output form container and js ajax call to load the actual form.
	 */ 
	function wck_fep_form_in_frontend( $atts ){	
		
		/* extract shortcode args */
		extract( shortcode_atts( array(
			'form_name' => ''			
		), $atts ) );
		
		/* the login form can't get the $_GET['loginerror'] from the url because it is loaded with ajax and we need to send it through post */ 
		if( isset( $_GET['loginerror'] ) )
			$loginerror = ', loginerror:\''. $_GET['loginerror'].'\'' ;
		else
			$loginerror = '';
		 
		/* set the static var to true */
		self::$wck_fep_add_scripts = true;
		
		/* media upload add here */
		wp_enqueue_media();
		
		$output = "<div class='". $form_name ." fep-container";
		if( isset( $_GET['action'] ) && $_GET['action'] == 'edit' )
			$output .=  " fep-edit";
		$output .= "'><div id='fep-ajax-loading'></div></div>";
		
		
		if( !empty( $_GET['action'] ) )
			$action = $_GET['action'];
		else
			$action = '';
			
		if( !empty( $_GET['post_id'] ) )
			$post_id = $_GET['post_id'];
		else
			$post_id = '';

        /* edit nonce */
        if( !empty( $_GET['_wpnonce'] ) ){
            $edit_nonce = $_GET['_wpnonce'];
        }
        else
            $edit_nonce = '';

        /* make sure we have jquery at this point */
        wp_print_scripts( 'jquery' );

		$output .= "<script type='text/javascript'>
		jQuery.post( wckAjaxurl,  { action:'wck_fep_create_frontend_form_".$form_name."', action_type:'".$action."', post_id:'".$post_id."', _wpnonce:'". $edit_nonce ."' ". $loginerror ."}, function(response) {
			jQuery('.fep-container.".$form_name."').html(response);
			jQuery( '#fep-ajax-loading' ).remove();
			jQuery(mb_sortable_elements);
		});		
		</script>";		
		
		
		return $output;
	}
	
	/**
	 * Function that builds the form
	 *
	 * @since 1.0.0	
	 */ 
	function wck_fep_create_frontend_form( ){
		
		/* check for anonymous posting */
		if( $this->args['anonymous_posting'] == 'no' && !is_user_logged_in() ){
			/* get login logout form */
			$lilo_form = wck_fep_output_lilo_form();			
			die( $lilo_form );
		}
		
		if( !empty( $_POST['action_type'] ) )
			$action = $_POST['action_type'];
		else
			$action = '';
		if( !empty( $_POST['post_id'] ) )
			$post_id = $_POST['post_id'];
		else
			$post_id = '';
		
		/* take care of auto_drafts only when adding posts ( the default case ) */
		if( $action == '' ){
			/* delete auto drafts older than 12 hours  */
			$args = array(
						'post_status' 		=> 	'auto-draft',
						'numberposts' 		=> 	-1,
						'suppress_filters' 	=> 	false,
						'post_type' 		=> 	'any' 
					);
			
			add_filter( 'posts_where', array( &$this, 'wck_fep_filter_auto_drafts_where' ) );
			$auto_drafts = get_posts( $args );
			remove_filter( 'posts_where', array( &$this, 'wck_fep_filter_auto_drafts_where' ) );
			
			
			if( !empty($auto_drafts) ){
				foreach( $auto_drafts as $auto_draft ){
				
					/* also delete all post meta for the auto-draft post */
					$meta_keys = get_post_custom_keys( $auto_draft->ID );				
					if( !empty($meta_keys) ){
						foreach( $meta_keys as $meta_key ){
							
							delete_post_meta( $auto_draft->ID, $meta_key );
						}
					}
					
					/* delete attachemnts for the auto draft */
					$args = array( 
						'post_parent' => $auto_draft->ID,
						'post_type'   => 'attachment', 
						'numberposts' => -1
					);
					$attachments = get_children( $args );
					if( !empty( $attachments ) ){
						foreach( $attachments as $attachment ){
							wp_delete_attachment( $attachment->ID );
						}
					}
					
					/* delete the post */
					wp_delete_post( $auto_draft->ID, true );
				}
			}
		}
		
		/* create a new post? not if we allready have a $post_id ( probabily edit case )*/			
		if ( !$post_id ) {
		global $post;
			$post_id = wp_insert_post( array( 'post_title' => __( 'Auto Draft' ), 'post_type' => $this->args['post_type'], 'post_status' => 'auto-draft' ) );
			$post = get_post( $post_id );			
		}
				
		/* create default post elements */
		$form = self::create_add_form( $this->args['meta_array'], $this->args['form_name'], $post_id, $action );

		echo $form;
		
		exit();
	}
	
	/**
	 * Filtering function that will add our where clause to the query
	 *
	 * @since 1.0.0	
	 */ 	
	function wck_fep_filter_auto_drafts_where( $where = '' ){
		// posts  older than 12 hours 
		$where .= " AND post_date <= '" . date('Y-m-d H:i:s', strtotime('-12 hours')) . "'";
		return $where;
	}	
	
	
	/**
	 * Function that creates the add form with the default fields
	 *
	 * @since 1.0.0	
	 *	 	 
	 * @param array $fields array of default fields	
	 * @param string $form_name the name of the form
	 * @param int $post_id the post ID
	 * @return string $output form container and js ajax call to load the actual form.
	 */
	function create_add_form( $fields, $form_name, $post_id, $action = '' ){
		$nonce = wp_create_nonce( 'wck-fep-add-post' );

        if( $action == 'edit'  ){

            if( $this->args['post_type'] != get_post_type( $post_id ) )
                return '<div class="fep-error fep-access-denied">' . __( "Wrong form for this post type", "wck" ) . '</div>';

            $fep_form = get_post( $post_id );
            if ( $fep_form ){
                $author_id = $fep_form->post_author;
                $user_ID = get_current_user_id();
                if ( !current_user_can( 'edit_others_posts' ) && !wp_verify_nonce( $_REQUEST['_wpnonce'], 'wck-fep-dashboard-edit-'.$post_id.'-'.$user_ID ) ){
                    if ($author_id != $user_ID) {
                        $error = '<div class="fep-error fep-access-denied">' . __( "You are not allowed to edit this post.", "wck" ) . '</div>';
                        return $error;
                    }
                }
            }
        }

        $form = '<form id="'. $form_name .'" method="post" action="">';
		
		$element_id = 0;		
		
		if( function_exists( 'wck_cfc_create_boxes_args' ) )
			$all_box_args = wck_cfc_create_boxes_args();
		
		if( !empty( $fields ) ){
			foreach( $fields as $details ){

				do_action( "wck_fep_before_form_{$form_name}_element_{$element_id}" );	

				$form .= apply_filters( "wck_fep_filter_before_form_{$form_name}_element_{$element_id}", '<div id="fep-'. Wordpress_Creation_Kit::wck_generate_slug( $details['title'] ) .'"  class="fep-element-wrap">', $details );
				
				if( empty( $details['cfc'] ) ){							
					
					if( $action == 'edit' ){
						/* build edit values depending on the field */
						$value = self::wck_fep_get_edit_value( $post_id, $details );					
					}
					else
						$value = apply_filters( 'wck_fep_default_value_'. $form_name . '_'. Wordpress_Creation_Kit::wck_generate_slug( $details['title'] ), '' );
					
					$form .= parent::wck_output_form_field( $form_name, $details, $value, 'fep', $post_id );
					
				}
				
				else{
				
					/* add CFC boxes created with Custom Fields Creator */					
					if( !empty( $all_box_args ) ){
						foreach( $all_box_args as $box_args ){						
							if( ( $box_args['post_type'] == $this->args['post_type'] ) && ( $box_args['metabox_title'] == $details['title']  ) ){						
								
								/* treat single cfcs case */
								if( $box_args['single'] ){								
									$form .= '<div id="'. $box_args['meta_name'].'" class="single-cfc">';								
									if( !empty( $box_args['meta_array'] ) ){
										foreach( $box_args['meta_array'] as $details ){
										
											if( $action == 'edit' ){					
												/* build edit values depending on the field */
												$value = self::wck_fep_get_edit_value( $post_id, $details, $box_args['meta_name'] );					
											}
											else
												$value = '';
											$form .= '<div class="fep-single-element-wrap">';
											$form .= parent::wck_output_form_field( $box_args['meta_name'], $details, $value, 'fep', $post_id );
											$form .= '</div>';
										}
									}
									$form .= '</div>';
								}							
								else{	
									ob_start();
									parent::create_add_form( $box_args['meta_array'], $box_args['meta_name'], get_post($post_id), 'fep' );
									$parent_form = ob_get_contents();
									ob_end_clean();
									
									$form .= $parent_form;	
									$form .= parent::wck_output_meta_content( $box_args['meta_name'], $post_id, $box_args['meta_array'], $box_args );								
								}							
							}			
						}
					}			
								
				}
                $form .= apply_filters( "wck_fep_filter_after_form_{$form_name}_element_{$element_id}", '</div>' );
				
				do_action( "wck_after_form_{$form_name}_element_{$element_id}" );
				
				$element_id++;
			}
		}
		
		
		if( $action == 'edit' )			
			$submit_text = apply_filters( 'wck_fep_form_button_update', __( 'Update Post', 'wck' ), $form_name );
		else 
			$submit_text = apply_filters( 'wck_fep_form_button_add', __( 'Add Post', 'wck' ), $form_name );
		
		$form .= '<input type="submit" id="submit_'.$form_name.'" value="'. $submit_text .'" onclick="wckFepAddPost(\''. $form_name .'\', '. $post_id .', \''. $action .'\', \''. $nonce .'\');return false;"/>';
		
		$form .= '</form>';		
		
		return $form;		
	}
	
	
	/**
	 * calls parrent function. till I figure out how to call parent function on ajax hook set up in child class
	 */
	function wck_add_meta(){
		parent::wck_add_meta();
	}
	
	/* function that gets the values of the default post fields for editing purpouses */
	function wck_fep_get_edit_value( $post_id, $details, $meta_name = '' ){
		$value = '';
		
		if( $meta_name == '' ){
			$post = get_post( $post_id );		
			switch ( $details['title'] ) {
				case 'Post Title':
					$value = get_the_title( $post->ID );
					break;
				case 'Post Content':
					$value = $post->post_content;
					break;
				case 'Post Excerpt':
					$value = $post->post_excerpt;
					break;
				case 'Featured Image':
					$value = get_post_thumbnail_id( $post->ID );
				default:
					/* take care of taxonomies */
					if( !empty( $this->args['taxonomies'] ) ){
						foreach( $this->args['taxonomies'] as $taxonomy ){
							
							if( $details['title'] == $taxonomy->label ){
								$object_terms = wp_get_object_terms( $post->ID, $taxonomy->name );
								
								if(!empty($object_terms)){
								  if(!is_wp_error( $object_terms )){
									foreach($object_terms as $term){
										$value[] = $term->name;
									}

									/* if not hyerarhical return string not array */
									if( !$taxonomy->hierarchical )
										$value = implode( ', ', $value );
								  }
								}
							}							
						}
					}					
			}
		}
		else{
			$meta_values = get_post_meta( $post_id, $meta_name, true );
			if( !empty( $meta_values ) )
				$value = $meta_values[0][Wordpress_Creation_Kit::wck_generate_slug( $details['title'] )];
		}
		
		return $value;
	}
	
	
	/**
	 * AJAX Function that ads the post
	 *
	 * @since 1.0.0		
	 */
	function wck_fep_add_post(){
		check_ajax_referer( 'wck-fep-add-post' );
		
		$meta = $_POST['meta'];
		$post_ID = $_POST['postid'];
		if( !empty( $_POST['values'] ) )
			$values = $_POST['values'];	
		else
			$values = array();
		$single_cfcs = (!empty( $_POST['single_cfcs'] )) ? $_POST['single_cfcs'] : array() ;
		$action_type = $_POST['action_type'];
		
		/* check required fields */	
		$errors = array();
		
		/* check required fields of single cfcs */		
		if( !empty( $single_cfcs ) ){
			foreach( $single_cfcs as $meta_name => $single_values ){
				/* check required fields */
				$errors[$meta_name] = self::wck_test_required( $this->single_cfcs[$meta_name], $meta_name, $single_values, 0 );			
			}			
		}
		
		/* check required fields for default post fields */
		$errors[$meta] = self::wck_test_required( $this->args['meta_array'], $meta, $values, 0 );
		
		/* combine the errors from the default fields with single cfcs */
		$errors_json = array( 'error' => '', 'errorfields' => array() );
		if( !empty( $errors ) ){
			foreach( $errors as $error ){
				if( !empty( $error ) ){
					$errors_json['error'] .= $error['error'];
					$errors_json['errorfields'] = array_merge( $errors_json['errorfields'], $error['errorfields'] );
				}
			}
		}
		
		if( !empty( $errors_json['error']) ){
			header( 'Content-type: application/json' );
			die( json_encode( $errors_json ) );
		} 
		
		/* if any single cfcs update the post meta */
		if( !empty( $single_cfcs ) ){
			foreach( $single_cfcs as $meta_name => $single_values ){
				update_post_meta( $post_ID, $meta_name, array( $single_values ) );
				
				/* if unserialize_fields is true add for each entry separate post meta for every element of the form  */				
				if( $this->single_cfcs[$meta_name.'_unserialize_fields'] ){					
					$meta_suffix = 1;
					if( !empty( $single_values ) ){
						foreach( $single_values as $name => $value ){
							update_post_meta( $post_ID, $meta_name.'_'.$name.'_'.$meta_suffix, $value );
						}
					}
				}
				
			}
		}
		
		/* treat empty keys in our $_POST values */
		$expected_keys = array('post-title', 'post-content', 'post-excerpt');
		foreach( $expected_keys as $expected_key ){
			if( !array_key_exists($expected_key, $values) ){
				$values[$expected_key] = '';
			}
		}
		
		$wck_fep_new_post = array(
			'ID' => $post_ID,
			'post_content' => $values['post-content'],
			'post_excerpt' => $values['post-excerpt'],
			'post_type' => $this->args['post_type']
		);


        if( !empty($values['post-title'] ) )
            $wck_fep_new_post['post_title'] = wp_strip_all_tags($values['post-title']);
        else {
            if( !empty( $action_type ) && $action_type == 'edit' )
                $wck_fep_new_post['post_title'] = get_the_title( $post_ID );
            else
                $wck_fep_new_post['post_title'] = '';
        }

        if( !empty($values['post-content'] ) )
            $wck_fep_new_post['post_content'] = wp_strip_all_tags($values['post-content']);
        else {
            if( !empty( $action_type ) && $action_type == 'edit' ) {
                $post_obj = get_post( $post_ID );
                $wck_fep_new_post['post_content'] = $post_obj->post_content;
            }
            else
                $wck_fep_new_post['post_content'] = '';
        }

        if( !empty($values['post-excerpt'] ) )
            $wck_fep_new_post['post_excerpt'] = wp_strip_all_tags($values['post-excerpt']);
        else {
            if( !empty( $action_type ) && $action_type == 'edit' ) {
                $post_obj = get_post( $post_ID );
                $wck_fep_new_post['post_excerpt'] = $post_obj->post_excerpt;
            }
            else
                $wck_fep_new_post['post_excerpt'] = '';
        }



        /* post status */
		if( $this->args['admin_approval'] == 'yes' )
			$wck_fep_new_post['post_status'] = 'draft';	
		else 
			$wck_fep_new_post['post_status'] = 'publish';

		/* post author */
		if( $this->args['anonymous_posting'] == 'yes' )
			$wck_fep_new_post['post_author'] = $this->args['assign_to_user'];	
		else 
			$wck_fep_new_post['post_author'] = get_current_user_id();
			
			
		if( !empty( $values['featured-image'] ) ){
			set_post_thumbnail( $post_ID, $values['featured-image'] );
		}	
		
		/* set comments status of the posts according to the default comment status */
		$comments_status = get_option( 'default_comment_status' );
		if( empty( $comments_status ) )
			$comments_status = 'closed';		
		$wck_fep_new_post['comment_status'] = $comments_status;

		// Insert the post into the database
		$post_id = wp_insert_post( $wck_fep_new_post );
		
		
		/* take care of taxonomies */
		if( !empty( $this->args['taxonomies'] ) ){
			foreach( $this->args['taxonomies'] as $taxonomy ){
				
				$tax_names = $values[ Wordpress_Creation_Kit::wck_generate_slug( $taxonomy->label ) ];
				
				if( !empty( $tax_names ) ){ 
					$tax_names = explode( ',', $tax_names );			
					
					/* strip white spaces */					
					if( !empty( $tax_names ) ){
						foreach( $tax_names as $key => $tax_name ){
							$tax_names[$key] = trim( $tax_name );
						}
					}
					
					/* get term ids by name and build the terms array */
					$terms = array();
					if( !empty( $tax_names ) ){
						foreach( $tax_names as $tax_name ){
							$idObj = get_term_by( 'name', htmlspecialchars( $tax_name ), $taxonomy->name ); 
							if( $idObj != false )
								$terms[] = $idObj->term_id;
							else{
								/* we don't have the term so create it ( for non hierarhical taxonomies ) */
								$insert = wp_insert_term( $tax_name, $taxonomy->name );
								if ( is_wp_error( $insert ) ) {
								   $error_string = $insert->get_error_message();
								   echo '<div id="message" class="error"><p>' . $error_string . '</p></div>';
								}
								else{
									$terms[] = $insert['term_id'];
								}
							}
						}
					}
					
					if( !empty( $terms ) ){					
						//to make sure the terms IDs are integers:
						$terms = array_map('intval', $terms);
						$terms = array_unique( $terms );					
						$set_result = wp_set_object_terms( intval($post_ID), $terms, $taxonomy->name );					
					}
				}
			}
		}
		
		if( $action_type == '' )
			do_action( 'wck_fep_add_post', $wck_fep_new_post, get_current_user_id() );
		else if( $action_type == 'edit' )
			do_action( 'wck_fep_update_post', $wck_fep_new_post, get_current_user_id() );
		
		if( $action_type == '' )		
			echo apply_filters( 'wck_fep_post_added_message', __( 'Post Added', 'wck' ), $meta );
		else if( $action_type == 'edit' )
			echo apply_filters( 'wck_fep_post_updated_message', __( 'Post Updated', 'wck' ), $meta );			
		
		die();
	}	
}

class WCK_FEP_Dashboard{	
	
	static $wck_fep_dashboard_add_scripts;
	
	/* Constructor method for the class. */
	function __construct( ) {	
		
		/* register the shortcode */
		add_shortcode( 'fep-dashboard', array( &$this, 'wck_fep_dashboard') );		
		
		/*print scripts*/
		add_action( 'init', array( &$this, 'wck_fep_dashboard_register_script' ) );
		add_action( 'wp_footer', array( &$this, 'wck_fep_dashboard_print_script' ) );
		
		// Set up the AJAX hooks
		add_action( "wp_ajax_wck_fep_delete_entry", array( &$this, 'wck_fep_dashboard_delete_post' ) );	
		add_action( 'wp_ajax_wck_fep_register_user', array( &$this, 'wck_fep_register_user' ) );
		add_action( 'wp_ajax_nopriv_wck_fep_register_user', array( &$this, 'wck_fep_register_user' ) );
		add_action( 'wp_ajax_wck_fep_update_user', array( &$this, 'wck_fep_update_user' ) );
		add_action( 'wp_ajax_nopriv_wck_fep_update_user', array( &$this, 'wck_fep_update_user' ) );		
	}
	
	static function wck_fep_dashboard_register_script(){
		/* FEP script */		
		wp_register_script( 'wck-fep', plugins_url('wck-fep.js', __FILE__ ), array('jquery'), '1.0', true );
		wp_register_style( 'wck-fep-css', plugins_url('wck-fep.css', __FILE__ ) );		
	}
	
	/**
	 * The function used to print scripts used by FEP in the footer of the page
	 *
	 * @since 1.0.0	
	 */
	static function wck_fep_dashboard_print_script() {
		/* if the static var set by the shortcode isn't true do nothing */
		if ( ! self::$wck_fep_dashboard_add_scripts )
			return;
 
		wp_print_scripts('jquery-ui-tabs');
		wp_print_scripts('wck-fep');		
		wp_print_styles('wck-fep-css');		
		
		/* ajax url for frontend */
		echo '<script type="text/javascript">var wckAjaxurl = "'. admin_url('admin-ajax.php') .'";</script>';		
	}
	

	/* FEP Dashboard */ 
	function wck_fep_dashboard(){
	
		/* set the static var to true */
		self::$wck_fep_dashboard_add_scripts = true;	
	
		if( !is_user_logged_in() ){
			return wck_fep_output_lilo_form();
		}
		
		
		$user_id = get_current_user_id();
		/* get post types */
		$args = array(
				'public'   => true
			);
		$output = 'objects'; // or objects
		$post_types = get_post_types($args,$output);
		
		$dashboard = "";	
		
		$dashboard .= '<div id="fep-dashboard">';

		$dashboard .= '<ul id="wck-fep-tabs">';		
		$dashboard .= apply_filters( 'wck_fep_dashboard_profile_tab', '<li><a href="#wck-fep-update-form">'. __( 'My Profile', 'wck' ) .'</a></li>' );
		if( !empty( $post_types ) ){
			foreach ($post_types  as $post_type ) {
				if ( $post_type->name != 'attachment' && $post_type->name != 'wck-meta-box' && $post_type->name != 'wck-frontend-posting' && $post_type->name != 'wck-option-page' && $post_type->name != 'wck-option-field' ){
					
					$args = array(
								'numberposts' => -1,
								'author' => $user_id,
								'post_type' => $post_type->name
							);
					$posts = get_posts( apply_filters( 'wck_fep_dashbord_get_posts_args', $args ) );

					if( !empty( $posts ) )				
						$dashboard .= '<li><a href="#fep-'. $post_type->name .'">'. __( 'My ', 'wck' ) .$post_type->label.'</a></li>';
				}
			}
		}
		$dashboard .= '</ul>';
		
		$dashboard .= wck_fep_output_user_profile();
		
		if( !empty( $post_types ) ){	
			foreach ($post_types  as $post_type ) {
				if ( $post_type->name != 'attachment' && $post_type->name != 'wck-meta-box' && $post_type->name != 'wck-frontend-posting' ){
					$args = array(
								'numberposts' => -1,
								'author' => $user_id,
								'post_type' => $post_type->name
							);
					$posts = get_posts( apply_filters( 'wck_fep_dashbord_get_posts_args', $args ) );
					if( !empty( $posts ) ){
						$dashboard .= '<div id="fep-'. $post_type->name .'">';
						$dashboard .= '<h3 class="fep-'. $post_type->name .'">'. __( 'My ', 'wck' ) . $post_type->label .'</h3>';
						$dashboard .= '<ul id="fep-'. $post_type->name .'">';				
						
						foreach( $posts as $post ){
							
							/* build edit link */
							$args = array(
								'post_type' => 'wck-frontend-posting',
								'numberposts' => -1
							);
							
							$all_forms = get_posts( $args );
                            $shortcode_page_id ='';
                            $edit_link='';
							if( !empty( $all_forms ) ){
								foreach( $all_forms as $form ){
									$wck_fep_args = get_post_meta( $form->ID, 'wck_fep_args', true );
									if( !empty( $wck_fep_args ) ){
										if( $wck_fep_args[0]['post-type'] == $post_type->name ){
											if( isset( $wck_fep_args[0]['shortcode-page'] ) ){
												$shortcode_page_id = $wck_fep_args[0]['shortcode-page'];
												break;
											}
										}
									}
								}
							}
							
							if( !empty( $shortcode_page_id ) ){
								$arr_params = array ( 'action' => 'edit', 'post_id' => $post->ID, '_wpnonce' => wp_create_nonce( 'wck-fep-dashboard-edit-'.$post->ID.'-'.$user_id ) );
								$edit_link = add_query_arg( $arr_params, get_permalink( $shortcode_page_id ) );
							}
							
						
							$dashboard .= '<li id="'. $post->ID .'"><a href="'. get_permalink( $post->ID ) .'">'. get_the_title( $post->ID ) .'</a>';
							if( !empty( $edit_link ) )
								$dashboard .= ' <a class="wck-edit-post" href="'.esc_url( $edit_link ).'">'. __( 'Edit', 'wck' ) .'</a> ';
								
							$delete_nonce = wp_create_nonce( 'wck-fep-delete-entry' );
							$dashboard .= ' <a class="wck-delete-post" href="javascript:void(0)" onclick="wckFepDeletePost(\''.$post->ID.'\', \''. $delete_nonce .'\')">'. __( 'Delete', 'wck' ) .'</a> </li>';
						}
						
						$dashboard .= '</ul></div>';				
					}
				}
				
			}
		}
		
		$dashboard .= '</div>';
		
		return $dashboard;	
	}
	
	static function wck_fep_dashboard_delete_post(){
		check_ajax_referer( "wck-fep-delete-entry" );
		if( !empty( $_POST['id'] ) )
			$id = absint($_POST['id']);
		else
			$id = '';
		
		if( wp_trash_post( $id ) !== false )
			die( 'Deleted' );
		else 
			die( 'Error Deleting' );
	}
	
	static function wck_fep_register_user(){
		wck_fep_handle_user_action();
	}
	
	static function wck_fep_update_user(){
		wck_fep_handle_user_action();
	}
}
new WCK_FEP_Dashboard();

/* login redirect filter. used to redirect from wp-login.php if it errors out */
add_filter( 'login_redirect', 'wck_fep_login_redirect', 10, 3 );
function wck_fep_login_redirect( $redirect_to, $redirect_url, $user ){
	/* if login action initialized by our form */
	if( isset( $_POST['wck-fep-login'] ) ){
		if( is_wp_error( $user ) ){
			$error_string = $user->get_error_message();
			$arr_params = array ( 'loginerror' => urlencode( base64_encode( $error_string ) ) );
			$redirect_to = add_query_arg( $arr_params, $redirect_to );
			wp_safe_redirect( $redirect_to );			
		}
		else{
			$redirect_to = remove_query_arg( 'loginerror', $redirect_to );
		}
	}
	return $redirect_to;
}

/* function to determine current page url */
function wck_fep_cur_page_url() {
	$pageURL = 'http';
	if (isset($_SERVER["HTTPS"]) && $_SERVER["HTTPS"] == "on") {$pageURL .= "s";}
	$pageURL .= "://";
	if ($_SERVER["SERVER_PORT"] != "80") {
		$pageURL .= $_SERVER["SERVER_NAME"].":".$_SERVER["SERVER_PORT"].$_SERVER["REQUEST_URI"];
	} else {		
		$pageURL .= $_SERVER["SERVER_NAME"].$_SERVER["REQUEST_URI"];
	}
	
	/* make sure if the function is called through ajax */
	if( $pageURL == admin_url('admin-ajax.php') )
		$pageURL = $_SERVER['HTTP_REFERER'];
	
	return $pageURL;
}

/**
 * Outputs login/register/logout 
 */
function wck_fep_output_lilo_form(){
	$lilo_form = '';
	if( !is_user_logged_in() ){		
		$nonce = wp_create_nonce( 'wck-fep-user-action' );
		
		$lilo_form .= '<div id="wck-fep-login-form">';
		
		$lilo_form .= '<div id="wck-fep-login-messages">';
		if ( isset( $_GET['loginerror'] ) || isset( $_POST['loginerror'] ) ){
			$loginerror = isset( $_GET['loginerror'] ) ? $_GET['loginerror'] : $_POST['loginerror'];
			$lilo_form .= '<span class="wck-fep-error">';
			$lilo_form .= urldecode( base64_decode( $loginerror ) );
			$lilo_form .= '</span>';
		}
				
		$lilo_form .= '</div>';
		
		$lilo_form .= '<form action="'. get_option('home') .'/wp-login.php" method="post"><ul>';
		$lilo_form .= '<li><label for="log">'. __('User:', 'wck').'</label>';
		$lilo_form .= '<input type="text" name="log" id="log" class="text-input" value="" /></li>';
		$lilo_form .= '<li><label for="password">'. __('Password:', 'wck') .'</label>';
		$lilo_form .= '<input type="password" name="pwd" id="pwd" class="text-input" /></li>';
		$lilo_form .= '<li><input type="submit"  name="submit" value="'. __('Log in', 'wck') .'" /></li>';
		$lilo_form .= '<li><div id="remember">
						<input class="remember-me checkbox" name="rememberme" id="rememberme" type="checkbox" checked="checked" value="forever" />
						<label for="rememberme" id="label-remember-me">'. __('Remember', 'wck').'</label>						
					   </div>';
		$lilo_form .= '<input type="hidden" value="'. wck_fep_cur_page_url() .'" name="redirect_to"/>';																							
		$lilo_form .= '<input type="hidden" value="wck-fep-login" name="wck-fep-login"/></li>';																							
		$lilo_form .= '</ul></form>';
		
		/* check if we have user registration opened */
		$registration_status = get_option( 'users_can_register' );
		if( $registration_status ){						
			$lilo_form .= '<span class="wck-fep-message">' . __( 'Don\'t have a user account? ', 'wck' ) . '</span>';			
			$lilo_form .= '<a href="javascript:void(0)" id="wck-fep-show-register-form">'. __('Register.', 'wck') .'</a>';
			$lilo_form .= '</div>';
			
			$lilo_form .= '<div id="wck-fep-register-form" style="display:none">';
			$lilo_form .= '<div id="wck-fep-registration-errors"></div>';
			$lilo_form .= '<ul>';
			$lilo_form .= '<li><label for="user-name">'. __('Username:', 'wck').'</label>';
			$lilo_form .= '<input type="text" name="user-name" id="user-name" class="text-input" value="" /></li>';
			$lilo_form .= '<li><label for="email">'. __('Email:', 'wck').'</label>';
			$lilo_form .= '<input type="text" name="email" id="email" class="text-input" value="" /></li>';	
			$lilo_form .= '<li><label for="password">'. __('Password:', 'wck').'</label>';
			$lilo_form .= '<input type="password" name="password" id="password" value="" /></li>';
			$lilo_form .= '<li><label for="confirm-password">'. __('Confirm Password:', 'wck').'</label>';
			$lilo_form .= '<input type="password" name="confirm-password" id="confirm-password" value="" /></li>';			
			$lilo_form .= '<li><input type="submit"  onclick="wckFepRegisterUser(\''. $nonce .'\')" name="register" id="register" value="Register" /></li>';
			$lilo_form .= '</ul>';
			$lilo_form .= '<a href="javascript:void(0)" id="wck-fep-back-to-login">'. __( 'Back to login.', 'wck' ) .'</a></div>';
		}
		else
			$lilo_form .= '</div>';
	}
	else{
		$current_user = wp_get_current_user();
		
		$lilo_form .= '<span class="wck-welcome">'. sprintf( __( 'Welcome %1$s! ', 'wck' ), $current_user->display_name ) . '</span>';
		$lilo_form .= '<a href="'. wp_logout_url( wck_fep_cur_page_url() ).'" class="wck-logout">'. __( 'Logout', 'wck' ) .'</a>';
	}
	
	return apply_filters( 'wck_fep_lilo_form', $lilo_form );
}

/**
 * Outputs the user profile update form
 */
function wck_fep_output_user_profile(){
	
	$nonce = wp_create_nonce( 'wck-fep-user-action' );
	
	$current_user = wp_get_current_user();
	$user_data = get_userdata( $current_user->ID );
	
	$user_profile = '<div id="wck-fep-update-form">';
	$user_profile .= '<p><span class="wck-welcome">'. sprintf( __( 'Welcome %1$s! ', 'wck' ), $current_user->display_name ) .'</span><a href="'. wp_logout_url( wck_fep_cur_page_url() ).'" class="wck-logout">'. __( 'Logout', 'wck' ) .'</a></p>';
	
	$user_profile .= '<p id="wck-fep-update-messages"></p>';
	$user_profile .= '<p id="wck-fep-update-errors"></p>';
	
	$user_profile .= '<ul>';	
	$user_profile .= '<li><label for="email">'. __( 'Email:', 'wck' ) .'</label>';
	$user_profile .= '<input type="text" id="email" name="email" value="'. esc_attr( $current_user->user_email ) .'" /></li>';
	
	$user_profile .= '<li><label for="password">'. __( 'Password:', 'wck' ) .'</label>';
	$user_profile .= '<input type="password" id="password" name="password"/></li>';
	
	$user_profile .= '<li><label for="confirm-password">'. __( 'Password Confirm:', 'wck' ) .'</label>';
	$user_profile .= '<input type="password" id="confirm-password" name="confirm-password"/></li>';
	
	$user_profile .= '<li><label for="description">'. __( 'Description', 'wck' ) .'</label>';
	$user_profile .= '<textarea id="description" name="description">'. esc_textarea( $user_data->user_description ) .'</textarea></li>';
	
	$user_profile .= '<li><input type="submit" value="'. __( 'Update', 'wck' ) .'" onclick="wckFepUpdateUser(\''. $nonce .'\')" /></li>';
	$user_profile .= '</ul></div>';
	
	return apply_filters( 'wck_fep_user_profile', $user_profile );
}

/**
 * Ajax callback for handling user register or update
 */
function wck_fep_handle_user_action(){
	
	check_ajax_referer( 'wck-fep-user-action' );
	
	if( !empty( $_POST['action_type'] ) )
		$action = $_POST['action_type'];
	else
		$action = '';
	if( !empty( $_POST['username'] ) )
		$username = $_POST['username'];
	else 
		$username = '';
	if( !empty( $_POST['email'] ) )
		$email = $_POST['email'];
	else 
		$email = '';
	if( !empty( $_POST['password'] ) )
		$password = $_POST['password'];
	else 
		$password = '';
	if( !empty( $_POST['confirm_password'] ) )
		$confirm_password = $_POST['confirm_password'];
	else 
		$confirm_password = '';
	if( !empty( $_POST['description'] ) )
		$description = $_POST['description'];
	else 
		$description = '';
	
	$registration_errors = new WP_Error();		
	
	if( $action == 'register' ){
		$username = sanitize_user( $username );
		// Check the username
		if ( $username == '' ) {
			$registration_errors->add( 'empty_username', __( 'Please enter a username.', 'wck' ) );
		} elseif ( ! validate_username( $username ) ) {
			$registration_errors->add( 'invalid_username', __( 'This username is invalid because it uses illegal characters. Please enter a valid username.', 'wck' ) );
			$username = '';
		} elseif ( username_exists( $username ) ) {
			$registration_errors->add( 'username_exists', __( 'This username is already registered, please choose another one.', 'wck' ) );
		}
	}

	// Check the e-mail address
	if ( $email == ''  ) {
		$registration_errors->add( 'empty_email', __( 'Please type your e-mail address.', 'wck' ) );
	} elseif ( ! is_email( $email ) ) {
		$registration_errors->add( 'invalid_email', __( 'The email address isn&#8217;t correct.', 'wck' ) );
		$email = '';
	} elseif ( email_exists( $email ) && $action== "register" ) {
		$registration_errors->add( 'email_exists', __( 'This email is already registered, please choose another one.', 'wck' ) );
	}
	
	// Check the password 
	if( empty( $password ) && $action== "register" )
		$registration_errors->add('password_error', __( 'Please enter a password.', 'wck' ) );
	else if( $password != $confirm_password )
		$registration_errors->add('password_dont_match_error', __( 'The passwords do not match.', 'wck' ) );

	$registration_errors = apply_filters('wck_registration_errors',$registration_errors, $_POST);

	if( $registration_errors->get_error_code() )
		$user = $registration_errors;

	if( empty( $user ) ){
		if( $action == 'register' ){
			$userdata = array(
						'user_login' => $username,
						'user_email' => $email,
						'user_pass' => $password						
					);
            $user = wp_insert_user( $userdata );
		}
		else if( $action == 'update' ){
			$current_user = wp_get_current_user();
			$userdata = array(
						'ID' => $current_user->ID,
						'user_login' => $current_user->user_login,
						'user_email' => $email,						
						'description' => $description
					);
			if( !empty( $password ) )
				$userdata['user_pass'] = $password;

            $user = wp_update_user( $userdata );
		}
				

	}
	
	if( is_wp_error( $user ) ){
		$error_messages = '';
		$user_error_messages = $user->get_error_messages();
		if( !empty( $user_error_messages ) ){
			foreach( $user_error_messages as $message ){
				$error_messages .= '<span class=wck-fep-error>'.$message.'</span><br />';
			}
		}
		die( $error_messages );
	}
	else die( 'User added successfully!' );				
}

/* basic redirect subscribers from admin to frontend */
add_action('admin_head', 'wck_fep_redirect_to_front_end');
function wck_fep_redirect_to_front_end() {	
	global $current_user;
	$user = new WP_User( $current_user->ID );
	if ( !empty( $user->roles ) && is_array( $user->roles ) ) {
		foreach ( $user->roles as $role ){
			if($role == 'subscriber'){
				if( wck_fep_cur_page_url() == admin_url() )
					wp_redirect(get_option('siteurl'));
			}
		}
	}	
}


/* Set up upload field for frontend */
/* overwrite the two functions for when an upload is made from the frontend so they don't check for a logged in user */
if( strpos( wp_get_referer(), 'wp-admin' ) === false && isset( $_REQUEST['action'] ) && 'upload-attachment' == $_REQUEST['action'] ){
		if( !function_exists( 'check_ajax_referer' ) ){
			function check_ajax_referer( ) {
				return true;
			}
		}
		
		if( !function_exists( 'auth_redirect' ) ){
			function auth_redirect() {
				return true;
			}
		}
}

/* create a fake user with the "upload_posts" capability and assign him to the global $current_user. this is used to bypass the checks for current_user_can('upload_files') in async-upload.php */
add_action( 'after_setup_theme', 'wck_create_fake_user_when_uploading_and_not_logged_in' );
function wck_create_fake_user_when_uploading_and_not_logged_in(){
	if( strpos( wp_get_referer(), 'wp-admin' ) === false && isset( $_REQUEST['action'] ) && 'upload-attachment' == $_REQUEST['action'] ){
        if( !is_user_logged_in() || !current_user_can('upload_files') || !current_user_can( 'edit_posts' ) ){
            global $current_user;
			$current_user = new WP_User( 0, 'frontend_uploader' );	
			$current_user->allcaps = array( "upload_files" => true, "edit_posts" => true, "edit_others_posts" => true, "edit_pages" => true, "edit_others_pages" => true );
		}
	}
}

/* for a request of a upload from the frontend and no user is logged in don't query for attachments */
add_action( 'after_setup_theme', 'wck_modify_query_attachements_when_not_logged_in' );
function wck_modify_query_attachements_when_not_logged_in(){
	if( strpos( wp_get_referer(), 'wp-admin' ) === false && !is_user_logged_in() ){
		add_action( 'wp_ajax_query-attachments', 'wck_wp_ajax_not_loggedin_query_attachments', 0 );
		add_action( 'wp_ajax_nopriv_query-attachments', 'wck_wp_ajax_not_loggedin_query_attachments', 0 );
		function wck_wp_ajax_not_loggedin_query_attachments(){
			wp_send_json_success( );
		}
	} 
}

/* restrict file types of the upload field functionality */
add_filter('wp_handle_upload_prefilter', 'wck_upload_file_type');
function wck_upload_file_type($file) {	
    if (isset($_POST['allowed_type']) && !empty($_POST['allowed_type'])){
        //this allows you to set multiple types seperated by a pipe "|"
        $allowed = explode("|", $_POST['allowed_type']);

        $ext =  substr(strrchr($file['name'],'.'),1);
        //first check if the user uploaded the right type
        if (!in_array($ext, (array)$allowed)){
            $file['error'] = __("Sorry, you cannot upload this file type for this field.");
            return $file;
        }
        //check if the type is allowed at all by WordPress
        foreach (get_allowed_mime_types() as $key => $value) {			
            if (strpos($key, $ext) !== false || $key == $ext)
                return $file;
        }
        $file['error'] = __("Sorry, you cannot upload this file type for this field.");
    }
    return $file;
}
?>
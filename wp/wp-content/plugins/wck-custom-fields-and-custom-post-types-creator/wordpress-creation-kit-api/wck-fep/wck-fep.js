/* add new post */
function wckFepAddPost(value, postId, action, nonce){
	
	/* add ajax loading */
	jQuery( '.fep-container' ).append( "<div id='fep-ajax-loading'></div>" );
	
	/* if CKEDITOR then trigger save. save puts the content in the hidden textarea */
	if( CKEDITOR !== undefined ){
		for ( instance in CKEDITOR.instances )
			CKEDITOR.instances[instance].updateElement();
	}	
		
	var values = wckGetTargetedValues( '#'+value+' .fep-element-wrap > .mb-right-column .mb-field' );
		
	/* check for single cfcs */
	if( jQuery('.single-cfc').length != 0 ){
		
		/*object to hold the singleCFCs */
		var singleCFCs = {};
		
		jQuery('.single-cfc').each(function(){
			key = jQuery(this).attr('id');
			
			singleValues = wckGetTargetedValues( '#'+key+' > .fep-single-element-wrap > .mb-right-column .mb-field' );
			singleCFCs[key] = singleValues;
		});
	}
	
	jQuery.post( wckAjaxurl,  { action:"wck_fep_add_post"+value, meta:value, postid:postId, values:values, single_cfcs:singleCFCs, action_type:action, _ajax_nonce:nonce}, function(response) {
			jQuery( '#fep-ajax-loading' ).remove();
			jQuery( '#'+value+' .fep-element-wrap > .mb-right-column .field-label').removeClass('error');
		
			if( response.error ){
				jQuery('#'+value).parent().css('opacity','1');
				jQuery('#mb-ajax-loading').remove();
				
				
				jQuery.each( response.errorfields, function (index, value) {
					jQuery( '#'+value+' .fep-element-wrap > .field-label[for="' + value + '"]' ).addClass('error');
				});			

				alert( response.error );
			}
			else{
				jQuery('.fep-container').replaceWith( response );
			}
			
		});			
}

/* helper function to store values of targets in an array */
function wckGetTargetedValues( target ){
	
	/*object to hold the values */
	var values = {};
	
	jQuery( target ).each(function(){		
		
		var key = jQuery(this).attr('name');		
		
		if(jQuery(this).attr('type') == 'checkbox' || jQuery(this).attr('type') == 'radio' ) {
			
			if( typeof values[key.toString()] === "undefined" )
				values[key.toString()] = '';
			
			if(jQuery(this).is(':checked')){
				if( values[key.toString()] == '' )
					values[key.toString()] += jQuery(this).val().toString();
				else
					values[key.toString()] += ', ' + jQuery(this).val().toString();
			}			
		}
		
		else		
			values[key.toString()] = jQuery(this).val().toString();			
		
	});	
	return values;
}

/* delete post */
function wckFepDeletePost( id, nonce){
	var response = confirm( "Delete this entry ?" );
	
	if( response == true ){
		elementParent = jQuery('#'+id).parent();
		elementParent.css({'opacity':'0.4', 'position':'relative'}).append('<div id="mb-ajax-loading"></div>');
		jQuery.post( wckAjaxurl ,  { action:"wck_fep_delete_entry", id:id, _ajax_nonce:nonce}, function(response) {
			if( response == 'Deleted' ){
				jQuery('#'+id).remove();
				jQuery('#mb-ajax-loading').remove();
				elementParent.css( {'opacity':'1'} );				
			}		
		});
	}
}

jQuery( function(){
		/* show register form */
		jQuery(document).on( 'click', '#wck-fep-show-register-form', function(){
			jQuery( '#wck-fep-login-form' ).hide();
			jQuery( '#wck-fep-register-form' ).show();
		});

		/* show login form */
		jQuery(document).on( 'click', '#wck-fep-back-to-login', function(){
			jQuery( '#wck-fep-login-form' ).show();
			jQuery( '#wck-fep-register-form' ).hide();
		});
});


/* register user function */
function wckFepRegisterUser( nonce ){
	username = jQuery( '#wck-fep-register-form #user-name' ).val();
	email = jQuery( '#wck-fep-register-form #email' ).val();
	password = jQuery( '#wck-fep-register-form #password' ).val();
	confirmPassword = jQuery( '#wck-fep-register-form #confirm-password' ).val();
	
	
	jQuery.post( wckAjaxurl,  { action:"wck_fep_register_user", action_type:'register', username:username, email:email, password:password, confirm_password:confirmPassword, _ajax_nonce:nonce }, function(response) {		
	
		if( response == 'User added successfully!' ){
			jQuery( '#wck-fep-login-messages' ).html( '<span class="wck-fep-message">' + response + '</span>' );
			jQuery( '#wck-fep-login-form' ).show();
			jQuery( '#wck-fep-register-form' ).hide();
		}
		else{
			jQuery('#wck-fep-registration-errors').html( response );
		}
		
		
	});
}

/* update user function */
function wckFepUpdateUser( nonce ){	
	email = jQuery( '#wck-fep-update-form #email' ).val();
	password = jQuery( '#wck-fep-update-form #password' ).val();
	confirmPassword = jQuery( '#wck-fep-update-form #confirm-password' ).val();
	description = jQuery( '#wck-fep-update-form #description' ).val();
	
	
	jQuery.post( wckAjaxurl,  { action:"wck_fep_update_user", action_type:'update', email:email, password:password, confirm_password:confirmPassword, description:description, _ajax_nonce:nonce }, function(response) {		
	
		if( response == 'User added successfully!' ){
			jQuery( '#wck-fep-update-messages' ).html( '<span class="wck-fep-message">Update Succesfull!</span>' );			
		}
		else{
			jQuery('#wck-fep-update-errors').html( response );
		}	
		
	});
}

/* fep dashboard init tabs */
jQuery(function(){
	if( jQuery( '#fep-dashboard' ).length != 0 )
		jQuery('#fep-dashboard').tabs();	
});
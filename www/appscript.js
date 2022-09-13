var stored;

$(document).keyup(function(event) {
  
  /* Ajouter tab */
  if ($('.tab-pane.active').has('#add-panel').length > 0) {

    if ($('#isbn').is(':focus') && (event.key == 'Enter')) {
      $('#isbnButton').click();
    }
    
    if ($('#add-panel input:focus').not('[haspopup=listbox]').length > 0 && (event.key == 'Enter')) {
      $('#add_button').click();
    }
    
    
  }

  /* Table tab */
  if ($('.tab-pane.active').has('#table_div').length > 0) {
  
    if (($('tr.active').length > 0) &&  
    $('#table_div input:focus').length == 0 &&
    $('#form-modal input:focus').length == 0) {
      
      if((event.key == 'e')) {
        Shiny.setInputValue('edit_button', true, {priority: 'event'});
      }
      
      if((event.key == 'd')) {
        Shiny.setInputValue('delete_button', true, {priority: 'event'});
      }
      
    }
    
    if ($('#form-modal input:focus').not('[haspopup=listbox]').length > 0 && (event.key == 'Enter')) {
      $('#submit_edit').click();
    }
    
  }
  
    /* Préférences tab */
  if ($('.tab-pane.active').has('#addvalues').length > 0) {

    if ($('#newchoice').is(':focus') && (event.key == 'Enter') && $('.modal-dialog').length == 0) {
      $('#add_choice_button').click();
    }
    
    if ($('.modal-dialog').length > 0) {
      $('#confirm_newchoice').click();
    }
    
  }
});

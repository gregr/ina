(function() {
  var logo_state, logo_swap;

  logo_state = 0;

  logo_swap = function() {
    if (logo_state === 0) {
      logo_state = 1;
      $('img.logoA').hide();
      $('img.logoB').show();
    } else {
      logo_state = 0;
      $('img.logoA').show();
      $('img.logoB').hide();
    }
    return setTimeout(logo_swap, 1000);
  };

}).call(this);

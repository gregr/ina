# this is to make the logos flash
logo_state = 0
logo_swap = () ->
  if logo_state == 0
    logo_state = 1
    $('img.logoA').hide()
    $('img.logoB').show()
  else
    logo_state = 0
    $('img.logoA').show()
    $('img.logoB').hide()

  setTimeout(logo_swap, 1000)
# $(logo_swap) # <-- do you hate people? if so uncomment this

$(document).ready ->
  goTo = (e) -> window.location = e if e

  $(document).on "keydown", (e) ->
    if e.keyCode in [37, 72]
      goTo($("#pback").attr("href"))
    if e.keyCode in [39, 76]
      goTo($("#pfor").attr("href"))

  $("[data-confirm]").on "click", ->
    confirm($(@).attr("data-confirm"))

  $("[data-tipsy]").tipsy
    opacity: 1
    gravity: ->
      if $(document).width() < 900 || $(@).offset().left <= 125
        'w'
      else
        'e'
    offset: 4
    trigger: 'focus'

window.disqus_shortname = 'otters'

window.DISQUSWIDGETS =
  displayCount: (obj) ->
    $.each(obj.counts, (_, c) ->
      badge = $("span[data-disqus='#{c.id}']")
      badge.html("#{c.comments} <span class='glyphicon glyphicon-comment'></span>")
      badge.addClass("commentful") if c.comments != 0
    )

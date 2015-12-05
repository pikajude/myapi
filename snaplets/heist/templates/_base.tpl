<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width,initial-scale=1">

    <title>jude.bio<subtitle/></title>

    <link rel="alternate" type="application/rss+xml" title="RSS feed" href="/.rss">
    <link rel="shortcut icon" href="/s/favicon.ico">
    <compile type="css">
      <css vendor src="foundation/css/normalize.css"/>
      <css vendor src="foundation/css/foundation.css"/>
      <css vendor src="font-awesome/css/font-awesome.css"/>
      <css vendor src="tipsy/src/stylesheets/tipsy.css"/>
      <sass src="sass/all.sass"/>
      <css src="css/amelie.css"/>
    </compile>
    <extra-head/>

    <!--[if lt IE 9]>
    <script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script>
    <![endif]-->
  </head>
  <body>
    <div class="row" role="main">
      <div class="speech large-12 columns">
        <flash type="success"/>
        <header>
          <a id="head" href="/">jude.bio</a>
          <span class="arrow"></span>
          <div id="dots">
            <span class="up-arrow"></span>
            <a href="https://github.com/pikajude" class="dot" id="github" data-tipsy title="I'm on GitHub!">I'm on GitHub!</a>
            <a href="http://www.linkedin.com/pub/joel-taylor/6a/691/988/" class="dot" id="linkedin" data-tipsy title="I'm on LinkedIn!">I'm on LinkedIn!</a>
            <ifLoggedIn>
              <a href="/n" class="dot" id="new-post" data-tipsy title="New post">New
  post</a>
            </ifLoggedIn>
          </div>
        </header>
        <apply-content/>

        <footer>
          Talk to me: <a href="mailto:me@jude.bio">me@jude.bio</a>.
        </footer>
      </div>
    </div>

    <compile type="javascript">
      <js vendor src="jquery/dist/jquery.js"/>
      <js vendor src="foundation/js/foundation.js"/>
      <js vendor src="tipsy/src/javascripts/jquery.tipsy.js"/>
      <coffee src="coffee/index.coffee"/>
      <coffee>$(document).foundation()</coffee>
    </compile>

    <extra-foot/>
  </body>
</html>

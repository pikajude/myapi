<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width,initial-scale=1">

    <title>jude.bio<subtitle/></title>

    <link rel="alternate" type="application/rss+xml" title="RSS feed" href="/.rss">
    <link rel="shortcut icon" href="/s/favicon.ico">
    <link rel="stylesheet" type="text/css" href="/vendor/foundation/css/normalize.min.css" />
    <link rel="stylesheet" type="text/css" href="/vendor/foundation/css/foundation.min.css" />
    <link rel="stylesheet" type="text/css" href="/vendor/font-awesome/css/font-awesome.min.css" />
    <link rel="stylesheet" type="text/css" href="/vendor/tipsy/src/stylesheets/tipsy.css" />
    <link rel="stylesheet" type="text/css" href="/css/all.css"/>
    <link rel="stylesheet" type="text/css" href="/s/css/amelie.css"/>

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

    <compile>
      <script src="/vendor/jquery/dist/jquery.js"/>
      <script src="/vendor/foundation/js/foundation.js"/>
      <script src="/vendor/tipsy/src/javascripts/jquery.tipsy.js"/>
      <script src="/js/index.js"></script>
      <script>$(document).foundation()</script>
    </compile>
    <extra-foot/>
  </body>
</html>

<singleEntry>
  <bind tag="subtitle"> » <postTitle/></bind>
  <bind tag="extra-foot">
    <script src="//otters.disqus.com/embed.js"></script>
    <compile>
      <script src="/js/disqus.js"></script>
    </compile>
  </bind>
  <apply template="base">
    <head>
      <link rel="stylesheet" href="/css/single.css" />
    </head>
    <article class="bubble blog-post">
      <h1 class="post-title">
        <postTitle/>

        <ifLoggedIn>
          <a class="edit-link fa fa-pencil" href="/e/${postId}"></a>
          <form class="delete-form" method="post" action="/d/${postId}">
            <input type="hidden" name="_method" value="DELETE" />
            <button type="submit" class="fa fa-trash-o" data-confirm="Are you sure you want to delete this post?"></button>
          </form>
        </ifLoggedIn>
      </h1>

      <postContent/>
    </article>

    <article class="bubble">
      <div id="disqus_thread"></div>
    </article>
  </apply>
</singleEntry>

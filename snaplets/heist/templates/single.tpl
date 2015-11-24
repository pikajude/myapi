<bind tag="subtitle"> » <postTitle/></bind>
<bind tag="extra-foot">
  <script src="//otters.disqus.com/embed.js"></script>
  <script src="/js/disqus.js"></script>
</bind>
<bind tag="extra-head">
  <link rel="stylesheet" href="/css/single.css" />
</bind>
<apply template="base">
  <article class="bubble blog-post">
    <h1 class="post-title">
      <postTitle/>

      <ifLoggedIn>
        <a class="edit-link fa fa-pencil" href="/e/${postId}"></a>
        <form class="delete-form" method="post" action="/d/${postId}">
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

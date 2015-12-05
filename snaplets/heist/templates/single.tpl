<singleEntry>
  <bind tag="subtitle"> » <postTitle/></bind>
  <bind tag="extra-foot">
    <compile type="javascript">
      <coffee src="coffee/disqus.coffee"/>
    </compile>
    <script src="//otters.disqus.com/embed.js"></script>
  </bind>
  <bind tag="extra-head">
    <compile type="css">
      <sass src="sass/single.sass"/>
    </compile>
  </bind>
  <apply template="_base">
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

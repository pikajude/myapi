<homePage>
  <bind tag="subtitle"/>
  <apply template="_base">
    <article class="bubble last-bubble">
      <h5 class="site-title">
        <ifLoggedIn>
          Hi buddy, nice website.
        </ifLoggedIn>

        <ifLoggedOut>
          I’m Jude, a functional programmer with a colorful head.
        </ifLoggedOut>
      </h5>
    </article>

    <postList>
      <article class="bubble preview-bubble">
        <h3 class="post-preview">
          <a class="post-title" href="/r/${postSlug}"><postTitle/></a>
        </h3>
      </article>
    </postList>
    <script src="${disqusUrl}"></script>
  </apply>

</homePage>

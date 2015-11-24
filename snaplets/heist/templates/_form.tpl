<apply template="base">
  <article class="bubble">
    <h2 class="form-title">Write something</h2>
    <form role="form" method="post" action="${formAction}">
      <apply-content/>
      <button type="submit" class="btn btn-default">Commit</button>
    </form>
  </article>
</apply>

<apply template="base">
  <article class="bubble">
    <h2 class="form-title">Write something</h2>
    <dfForm action="${formAction}">
      <dfChildErrorList ref="" />

      <apply-content/>

      <button type="submit" class="btn btn-default">Commit</button>
    </dfForm>
  </article>
</apply>

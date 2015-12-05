<bind tag="extra-head">
  <compile type="css">
    <sass src="sass/form.sass"/>
  </compile>
</bind>
<apply template="_base">
  <article class="bubble">
    <h2 class="form-title">Write something</h2>
    <entryForm action="${formAction}">
      <div class="form-group">
        <dfLabel ref="title">
          Title
          <dfInputText ref="title" class="form-control" />
        </dfLabel>
        <dfIfChildErrors ref="title">
          <small class="error"><dfErrorList ref="title" /></small>
        </dfIfChildErrors>
      </div>

      <div class="form-group">
        <dfLabel ref="content">
          Content
          <dfInputTextArea ref="content" class="form-control" rows="10" />
        </dfLabel>
        <dfIfChildErrors ref="content">
          <small class="error"><dfErrorList ref="content" /></small>
        </dfIfChildErrors>
      </div>

      <button type="submit" class="btn btn-default">Commit</button>
    </entryForm>
  </article>
</apply>

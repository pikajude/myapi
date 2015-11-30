<editEntry>
  <bind tag="subtitle"> » Editing &lsquo;<postTitle/>&rsquo;</bind>
  <bind tag="extra-foot"/>
  <apply template="_form">
    <head>
      <link href="/css/form.css" rel="stylesheet" type="text/css" />
    </head>
    <entryForm action="/e/${postId}">
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
  </apply>
</editEntry>

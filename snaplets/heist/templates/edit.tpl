<bind tag="subtitle"> » Editing &lsquo;<postTitle/>&rsquo;</bind>
<bind tag="extra-foot"/>
<bind tag="extra-head">
  <link href="/css/form.css" rel="stylesheet" type="text/css" />
</bind>
<apply template="_form">
  <div class="form-group">
    <label for="title">
      Title
      <input name="title" class="form-control" type="text" value="${postTitle}" />
    </label>
  </div>

  <div class="form-group">
    <label for="content">
      Content
      <textarea name="content" class="form-control" rows="10">
        <postContentRaw/>
      </textarea>
    </label>
  </div>
</apply>

<bind tag="subtitle"> » Editing &lsquo;<postTitle/>&rsquo;</bind>
<bind tag="extra-foot"/>
<bind tag="extra-head">
  <link href="/css/form.css" rel="stylesheet" type="text/css" />
</bind>
<apply template="_form">
  <div class="form-group">
    <dfLabel ref="title">
      Title
      <dfInputText ref="title" class="form-control" />
    </dfLabel>
  </div>

  <div class="form-group">
    <dfLabel ref="content">
      Content
      <dfInputTextArea ref="content" class="form-control" rows="10" />
    </dfLabel>
  </div>
</apply>

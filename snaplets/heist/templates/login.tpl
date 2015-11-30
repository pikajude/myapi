<bind tag="extra-head">
  <link rel="stylesheet" type="text/css" href="/css/login.css" />
</bind>
<apply template="base">
  <article class="bubble">
    <h3 class="form-title">Log in</h3>
    <form role="form" method="post" action="/in">
      <div class="row">
        <div class="large-6 columns">
          <label>
            Email
            <input name="login" type="text" class="form-control" />
          </label>
        </div>

        <div class="large-6 columns">
          <label>
            Password
            <input name="password" type="password" class="form-control" />
          </label>
        </div>
      </div>
      <button type="submit" class="button tiny">Try it</button>
    </form>
  </article>

<!--
  <h1>Snap Example App Login</h1>

  <p><loginError/></p>

  <bind tag="postAction">/in</bind>
  <bind tag="submitText">Login</bind>
  <apply template="userform"/>
-->
</apply>

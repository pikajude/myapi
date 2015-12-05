<bind tag="extra-head">
  <compile type="css">
    <sass src="sass/form.sass"/>
  </compile>
</bind>
<bind tag="extra-foot"/>
<apply template="_base">
  <article class="bubble">
    <h3 class="form-title">Log in</h3>
    <loginForm role="form" method="post" action="/in">
      <dfIfChildErrors>
        <small class="error"><dfErrorList ref="" /></small>
      </dfIfChildErrors>

      <div class="row">
        <div class="large-6 columns">
          <dfLabel ref="username">
            Username
            <dfInputText ref="username" class="form-control" />
          </dfLabel>
        </div>

        <div class="large-6 columns">
          <dfLabel ref="password">
            Password
            <dfInputPassword ref="password" class="form-control" />
          </dfLabel>
        </div>
      </div>
      <button type="submit" class="button tiny">Try it</button>
    </loginForm>
  </article>
</apply>

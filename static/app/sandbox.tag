const test = require('./cards/core/test.tag')

<sandbox>

  <header>
    <div class="control width" show={card}>
      <h2>width</h2>
    </div>
    <div class="control height" show={card}>
      <h2>height</h2>
    </div>
    <h1><a href="/sandbox">χ</a></h1>
  </header>

  <div class="area">
    <div>{ test.fragment }</div>
    <sandbox-fragment onclick={ click } />

    <test />
    <test />

  </div>

  <style>
    * {
      margin: 0;
      padding: 0;
      border: none;
    }
    *, *:before, *:after {
      transition: all 0.2s ease;
    }

    body {
      background-color: #F5F5F5;
      font-family: sans-serif;
    }

    a {
      text-decoration: none;
    }
  </style>

  <style scoped>

    :scope > header {
      position: fixed;
        top: 0;
        left: 0;
        right: 0;
      font-size: 24px;
      height: 48px;
      width: auto;
      padding: 24px;
    }
    :scope > header h1 {
      margin: auto;
      text-align: center;

      height: 48px;
      width: 48px;

      line-height: 48px;
      font-size: 30px;
      font-weight: normal;
      font-family: serif;
      font-style: italic;
    }
    :scope > header h1 a {
      display: block;
      width: 48px;
      height: 48px;
      color: #999;
    }
    :scope > header h1 a:hover {
      color: #DDD;
    }

    :scope > header .control {
      height: 48px;
    }
    :scope > header .control.width {
      float: left;
    }
    :scope > header .control.height {
      float: right;
    }
    :scope > header h2 {
      font-size: 10px;
      line-height: 24px;
      font-weight: normal;
      text-transform: uppercase;
      color: #999;
    }

    :scope > .area {
      position: absolute;
        top: 96px;
        left: 0;
        right: 0;
        bottom: 0;
      margin: auto;
      overflow: auto;
    }

  </style>

  <script>

    this.card = null
    this.test = test

    click(e) {
      riot.route('/sandbox/test/?w=auto&h=auto')
    }

    // router

    riot.route('/sandbox', () => {
      console.log('index route')
      this.card = null
      this.update()
    })

    riot.route('/sandbox/*/..', card => {
      let q = riot.route.query()
      console.log('want to render component', card, q)
      this.card = card
      this.update()
    })

    riot.route.base('/')
    riot.route.start(true)

  </script>

</sandbox>

<sandbox-fragment>

  <div>hello { world }</div>
  <div> t e s t </div>

  <script>
    this.world = 10
  </script>

</sandbox-fragment>

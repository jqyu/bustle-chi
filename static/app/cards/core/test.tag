module.exports.fragment = `... on Test {
  foo
  bar
}`

<test>

  <h2>test</h2>
  <p>this is a test counter card { counter }</p>
  <button onclick="{ add }">increment</button>

  <style scoped>
    :scope {
      display: block;
      background-color: green;
      color: white;
      font-family: sans-serif;
      padding: 2em;
      margin: 2em;
    }
    button {
      padding: 1em;
    }
  </style>

  <script>
    this.counter = 0

    add(e) {
      this.counter++
    }
  </script>

</test>

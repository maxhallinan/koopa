const CodeMirror = require('codemirror');

exports._initCodeMirror = function (element, value) {
  const codeMirror = CodeMirror(element, {
    lineNumbers: true,
    tabSize: 2,
    value,
  });

  return codeMirror;
};

exports._onChange = function (codeMirror, handler) {
  codeMirror.on("changes", function () {
    const value = codeMirror.getValue();
    // `handler` returns an Effect, which is a function that must be called to 
    // perform the effect.
    handler(value)();
  });
};

const CodeMirror = require('codemirror');
require('codemirror/addon/selection/active-line.js')

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

exports._setCursor = function (srcPos, codeMirror) {
  codeMirror.setCursor({
    ch: srcPos.column,
    line: srcPos.line,
  });
};

exports._styleActiveLine = function (isStyled, codeMirror) {
  codeMirror.setOption("styleActiveLine", isStyled);
};

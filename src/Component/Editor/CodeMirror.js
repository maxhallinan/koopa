const CodeMirror = require('codemirror');

exports._initCodeMirror = function (element, value) {
  const editor = CodeMirror(element, {
    lineNumbers: true,
    tabSize: 2,
    value,
  });

  return editor;
};

<!DOCTYPE html>

<html>
<head>
  <title>${title}</title>
  <meta http-equiv="content-type" content="text/html; charset=UTF-8">
  <!--<link rel="stylesheet" media="all" href="docco.css" />-->
  <style>
/*--------------------- Layout and Typography ----------------------------*/
body {
  font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, FreeSerif, serif;
  font-size: 15px;
  line-height: 22px;
  color: #dcdccc;
  margin: 0; padding: 0;
  background-color: #2b2b2b;
}
a {
  color: #261a3b;
}
  a:visited {
    color: #261a3b;
  }
p {
  margin: 0 0 15px 0;
}
h1, h2, h3, h4, h5, h6 {
  margin: 0px 0 15px 0;
}
  h1 {
    margin-top: 40px;
  }
#container {
  position: relative;
}
#background {
  position: fixed;
  top: 0; left: 525px; right: 0; bottom: 0;
  background: #2f2f2f;
  border-left: 1px solid #e5e5ee;
  z-index: -1;
}
#jump_to, #jump_page {
  background: white;
  -webkit-box-shadow: 0 0 25px #777; -moz-box-shadow: 0 0 25px #777;
  -webkit-border-bottom-left-radius: 5px; -moz-border-radius-bottomleft: 5px;
  font: 10px Arial;
  text-transform: uppercase;
  cursor: pointer;
  text-align: right;
}
#jump_to, #jump_wrapper {
  position: fixed;
  right: 0; top: 0;
  padding: 5px 10px;
}
  #jump_wrapper {
    padding: 0;
    display: none;
  }
    #jump_to:hover #jump_wrapper {
      display: block;
    }
    #jump_page {
      padding: 5px 0 3px;
      margin: 0 0 25px 25px;
    }
      #jump_page .source {
        display: block;
        padding: 5px 10px;
        text-decoration: none;
        border-top: 1px solid #eee;
      }
        #jump_page .source:hover {
          background: #f5f5ff;
        }
        #jump_page .source:first-child {
        }
table td {
  border: 0;
  outline: 0;
}
  td.docs, th.docs {
    max-width: 450px;
    min-width: 450px;
    min-height: 5px;
    padding: 10px 25px 1px 50px;
    overflow-x: hidden;
    vertical-align: top;
    text-align: left;
  }
    .docs pre {
      margin: 15px 0 15px;
      padding-left: 15px;
    }
    .docs p tt, .docs p code {
      background: #2b2b2b;
      border: 1px solid #dedede;
      font-size: 12px;
      padding: 0 0.2em;
    }
    .pilwrap {
      position: relative;
    }
      .pilcrow {
        font: 12px Arial;
        text-decoration: none;
        color: #454545;
        position: absolute;
        top: 3px; left: -20px;
        padding: 1px 2px;
        opacity: 0;
        -webkit-transition: opacity 0.2s linear;
      }
        td.docs:hover .pilcrow {
          opacity: 1;
        }
  td.code, th.code {
    padding: 10px 15px 16px 25px;
    width: 100%;
    vertical-align: top;
    background: #2b2b2b;
    border-left: 1px solid #e5e5ee;
  }
    pre, tt, code {
      font-size: 12px; line-height: 18px;
      font-family: Menlo, Monaco, Consolas, "Lucida Console", monospace;
      margin: 0; padding: 0;
    }


/*---------------------- Syntax Highlighting -----------------------------*/
pre, code {
    color: #fdce93;
    background-color: #2b2b2b;
  }

  .hll { background-color: #222222 }
  .c { color: #7f9f7f } /* Comment */
  .err { color: #e37170; background-color: #3d3535; } /* Error */
  .g { color: #7f9f7f } /* Generic */
  .k { color: #f0dfaf } /* Keyword */
  .l { color: #cccccc } /* Literal, TODO */
  .n { color: #dcdccc } /* Name */
  .o { color: #f0efd0 } /* Operator */
  .x { color: #cccccc } /* Other */
  .p { color: #41706f } /* Punctuation */
  .cm { color: #7f9f7f } /* Comment.Multiline */
  .cp { color: #7f9f7f } /* Comment.Preproc */
  .c1 { color: #7f9f7f } /* Comment.Single */
  .cs { color: #cd0000; font-weight: bold } /* Comment.Special */
  .gd { color: #cd0000 } /* Generic.Deleted */
  .ge { color: #cccccc; font-style: italic } /* Generic.Emph */
  .gr { color: #FF0000 } /* Generic.Error */
  .gh { color: #dcdccc; font-weight: bold } /* Generic.Heading */
  .gi { color: #00cd00 } /* Generic.Inserted */
  .go { color: #808080 } /* Generic.Output */
  .gp { color: #dcdccc; font-weight: bold } /* Generic.Prompt */
  .gs { color: #cccccc; font-weight: bold } /* Generic.Strong */
  .gu { color: #800080; font-weight: bold } /* Generic.Subheading */
  .gt { color: #0040D0 } /* Generic.Traceback */
  .kc { color: #dca3a3 } /* Keyword.Constant */
  .kd { color: #ffff86 } /* Keyword.Declaration */
  .kn { color: #dfaf8f; font-weight: bold; } /* Keyword.Namespace */
  .kp { color: #cdcf99 } /* Keyword.Pseudo */
  .kr { color: #cdcd00 } /* Keyword.Reserved */
  .kt { color: #00cd00 } /* Keyword.Type */
  .ld { color: #cc9393 } /* Literal.Date */
  .m { color: #8cd0d3 } /* Literal.Number */
  .s { color: #cc9393 } /* Literal.String */
  .na { color: #9ac39f } /* Name.Attribute */
  .nb { color: #efef8f } /* Name.Builtin */
  .nc { color: #efef8f } /* Name.Class */
  .no { color: #cccccc } /* Name.Constant */
  .nd { color: #cccccc } /* Name.Decorator */
  .ni { color: #c28182 } /* Name.Entity */
  .ne { color: #c3bf9f; font-weight: bold } /* Name.Exception */
  .nf { color: #efef8f } /* Name.Function */
  .nl { color: #cccccc } /* Name.Label */
  .nn { color: #8fbede } /* Name.Namespace */
  .nx { color: #cccccc } /* Name.Other */
  .py { color: #cccccc } /* Name.Property */
  .nt { color: #9ac39f } /* Name.Tag */
  .nv { color: #dcdccc } /* Name.Variable */
  .ow { color: #f0efd0 } /* Operator.Word */
  .w { color: #cccccc } /* Text.Whitespace */
  .mf { color: #8cd0d3 } /* Literal.Number.Float */
  .mh { color: #8cd0d3 } /* Literal.Number.Hex */
  .mi { color: #8cd0d3 } /* Literal.Number.Integer */
  .mo { color: #8cd0d3 } /* Literal.Number.Oct */
  .sb { color: #cc9393 } /* Literal.String.Backtick */
  .sc { color: #cc9393 } /* Literal.String.Char */
  .sd { color: #cc9393 } /* Literal.String.Doc */
  .s2 { color: #cc9393 } /* Literal.String.Double */
  .se { color: #cc9393 } /* Literal.String.Escape */
  .sh { color: #cc9393 } /* Literal.String.Heredoc */
  .si { color: #cc9393 } /* Literal.String.Interpol */
  .sx { color: #cc9393 } /* Literal.String.Other */
  .sr { color: #cc9393 } /* Literal.String.Regex */
  .s1 { color: #cc9393 } /* Literal.String.Single */
  .ss { color: #cc9393 } /* Literal.String.Symbol */
  .bp { color: #efef8f } /* Name.Builtin.Pseudo */
  .vc { color: #efef8f } /* Name.Variable.Class */
  .vg { color: #dcdccc } /* Name.Variable.Global */
  .vi { color: #ffffc7 } /* Name.Variable.Instance */
  .il { color: #8cd0d3 } /* Literal.Number.Integer.Long */

  </style>
</head>
<body>
  <div id="container">
    <div id="background"></div>
    <table cellpadding="0" cellspacing="0">
      <thead>
        <tr>
          <th class="docs">
            <h1>
              ${title}
            </h1>
          </th>
          <th class="code">
          </th>
        </tr>
      </thead>
      <tbody>
        % for section in sections:
          <tr id="section-${section[0]}">
            <td class="docs">
              <div class="pilwrap">
                <a class="pilcrow" href="#section-${section[0]}">&#182;</a>
              </div>
              ${section[1]}
            </td>
            <td class="code">
              ${section[2]}
            </td>
          </tr>
        % endfor
      </tbody>
    </table>
  </div>
</body>
</html>

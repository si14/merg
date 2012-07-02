import houdini
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from erlport import Port, Protocol
import misaka as m
from misaka import Markdown, HtmlRenderer, SmartyPants
from mako.template import Template

from itertools import groupby

class CodeRenderer(HtmlRenderer, SmartyPants):
    def block_code(self, text, lang):
        if not lang:
            return '\n<pre><code>%s</code></pre>\n' % \
                houdini.escape_html(text.strip())
        lexer = get_lexer_by_name(lang, stripall=True)
        formatter = HtmlFormatter()
        return highlight(text, lexer, formatter)

erl_lexer = get_lexer_by_name("erlang")
formatter = HtmlFormatter()
rndr = CodeRenderer()
md = m.Markdown(rndr, extensions=(m.EXT_FENCED_CODE
                                  | m.EXT_NO_INTRA_EMPHASIS))

def parse_sections(code):
    has_code = False
    i = 0
    sections = []
    doc_text = []
    code_text = []
    for line in code.strip().split("\n"):
        if line.lstrip().startswith("%"):
            if has_code:
                sections.append((i, "\n".join(doc_text), "\n".join(code_text)))
                code_text = []
                doc_text = []
                has_code = False
                i += 1
            doc_text.append(line.lstrip(" %"))
        else:
            has_code = True
            code_text.append(line)
    sections.append((i, "\n".join(doc_text), "\n".join(code_text)))
    return sections

class MergProtocol(Protocol):
    def handle_init(self, template_file):
        self.tmpl = Template(filename=template_file)
        return "ok"

    def handle_process(self, filename, code):
        sections = parse_sections(code)
        htmled_sections = [(idx, md.render(doc),
                            highlight(code, erl_lexer, formatter))
                           for idx, doc, code in sections]
        result = self.tmpl.render(title=filename, sections=htmled_sections)
        return result

if __name__ == "__main__":
    proto = MergProtocol()
    proto.run(Port(packet=4, use_stdio=False))

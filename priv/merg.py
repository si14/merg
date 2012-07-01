import houdini
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter
from erlport import Port, Protocol
import misaka as m
from misaka import Markdown, HtmlRenderer, SmartyPants

from itertools import groupby

MERG_COMMENT = "%%- "

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
    sections = []
    doc_text = []
    code_text = []
    for line in code.strip().split("\n"):
        lstripped = line.lstrip()
        if lstripped.startswith(MERG_COMMENT):
            if has_code:
                sections.append(("\n".join(doc_text), "\n".join(code_text)))
                code_text = []
                doc_text = []
                has_code = False
            doc_text.append(lstripped[len(MERG_COMMENT):])
        else:
            has_code = True
            code_text.append(line)
    sections.append(("\n".join(doc_text), "\n".join(code_text)))
    return sections

class MergProtocol(Protocol):
    def handle_process(self, code):
        sections = parse_sections(code)
        htmled_sections = [(md.render(doc), highlight(code, erl_lexer, formatter))
                           for doc, code in sections]
        print htmled_sections


        result = highlight(code, erl_lexer, formatter)
        return result


if __name__ == "__main__":
    proto = MergProtocol()
    proto.run(Port(packet=4, use_stdio=False))

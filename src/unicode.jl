
## todo: parse and include
## https://www.unicode.org/Public/UNIDATA/Scripts.txt
## unicode_scripts


unicode_blocks = [
    ("Basic_Latin", 0x0000:0x007F),
    ("Latin-1_Supplement", 0x0080:0x00FF),
    ("Latin_Extended-A", 0x0100:0x017F),
    ("Latin_Extended-B", 0x0180:0x024F),
    ("IPA_Extensions", 0x0250:0x02AF),
    ("Spacing_Modifier_Letters", 0x02B0:0x02FF),
    ("Combining_Diacritical_Marks", 0x0300:0x036F),
    ("Greek_and_Coptic", 0x0370:0x03FF),
    ("Cyrillic", 0x0400:0x04FF),
    ("Cyrillic_Supplementary", 0x0500:0x052F),
    ("Armenian", 0x0530:0x058F),
    ("Hebrew", 0x0590:0x05FF),
    ("Arabic", 0x0600:0x06FF),
    ("Syriac", 0x0700:0x074F),
    ("Thaana", 0x0780:0x07BF),
    ("Devanagari", 0x0900:0x097F),
    ("Bengali", 0x0980:0x09FF),
    ("Gurmukhi", 0x0A00:0x0A7F),
    ("Gujarati", 0x0A80:0x0AFF),
    ("Oriya", 0x0B00:0x0B7F),
    ("Tamil", 0x0B80:0x0BFF),
    ("Telugu", 0x0C00:0x0C7F),
    ("Kannada", 0x0C80:0x0CFF),
    ("Malayalam", 0x0D00:0x0D7F),
    ("Sinhala", 0x0D80:0x0DFF),
    ("Thai", 0x0E00:0x0E7F),
    ("Lao", 0x0E80:0x0EFF),
    ("Tibetan", 0x0F00:0x0FFF),
    ("Myanmar", 0x1000:0x109F),
    ("Georgian", 0x10A0:0x10FF),
    ("Hangul_Jamo", 0x1100:0x11FF),
    ("Ethiopic", 0x1200:0x137F),
    ("Cherokee", 0x13A0:0x13FF),
    ("Unified_Canadian_Aboriginal_Syllabics", 0x1400:0x167F),
    ("Ogham", 0x1680:0x169F),
    ("Runic", 0x16A0:0x16FF),
    ("Tagalog", 0x1700:0x171F),
    ("Hanunoo", 0x1720:0x173F),
    ("Buhid", 0x1740:0x175F),
    ("Tagbanwa", 0x1760:0x177F),
    ("Khmer", 0x1780:0x17FF),
    ("Mongolian", 0x1800:0x18AF),
    ("Limbu", 0x1900:0x194F),
    ("Tai_Le", 0x1950:0x197F),
    ("Khmer_Symbols", 0x19E0:0x19FF),
    ("Phonetic_Extensions", 0x1D00:0x1D7F),
    ("Latin_Extended_Additional", 0x1E00:0x1EFF),
    ("Greek_Extended", 0x1F00:0x1FFF),
    ("General_Punctuation", 0x2000:0x206F),
    ("Superscripts_and_Subscripts", 0x2070:0x209F),
    ("Currency_Symbols", 0x20A0:0x20CF),
    ("Combining_Diacritical_Marks_for_Symbols", 0x20D0:0x20FF),
    ("Letterlike_Symbols", 0x2100:0x214F),
    ("Number_Forms", 0x2150:0x218F),
    ("Arrows", 0x2190:0x21FF),
    ("Mathematical_Operators", 0x2200:0x22FF),
    ("Miscellaneous_Technical", 0x2300:0x23FF),
    ("Control_Pictures", 0x2400:0x243F),
    ("Optical_Character_Recognition", 0x2440:0x245F),
    ("Enclosed_Alphanumerics", 0x2460:0x24FF),
    ("Box_Drawing", 0x2500:0x257F),
    ("Block_Elements", 0x2580:0x259F),
    ("Geometric_Shapes", 0x25A0:0x25FF),
    ("Miscellaneous_Symbols", 0x2600:0x26FF),
    ("Dingbats", 0x2700:0x27BF),
    ("Miscellaneous_Mathematical_Symbols-A", 0x27C0:0x27EF),
    ("Supplemental_Arrows-A", 0x27F0:0x27FF),
    ("Braille_Patterns", 0x2800:0x28FF),
    ("Supplemental_Arrows-B", 0x2900:0x297F),
    ("Miscellaneous_Mathematical_Symbols-B", 0x2980:0x29FF),
    ("Supplemental_Mathematical_Operators", 0x2A00:0x2AFF),
    ("Miscellaneous_Symbols_and_Arrows", 0x2B00:0x2BFF),
    ("CJK_Radicals_Supplement", 0x2E80:0x2EFF),
    ("Kangxi_Radicals", 0x2F00:0x2FDF),
    ("Ideographic_Description_Characters", 0x2FF0:0x2FFF),
    ("CJK_Symbols_and_Punctuation", 0x3000:0x303F),
    ("Hiragana", 0x3040:0x309F),
    ("Katakana", 0x30A0:0x30FF),
    ("Bopomofo", 0x3100:0x312F),
    ("Hangul_Compatibility_Jamo", 0x3130:0x318F),
    ("Kanbun", 0x3190:0x319F),
    ("Bopomofo_Extended", 0x31A0:0x31BF),
    ("Katakana_Phonetic_Extensions", 0x31F0:0x31FF),
    ("Enclosed_CJK_Letters_and_Months", 0x3200:0x32FF),
    ("CJK_Compatibility", 0x3300:0x33FF),
    ("CJK_Unified_Ideographs_Extension_A", 0x3400:0x4DBF),
    ("Yijing_Hexagram_Symbols", 0x4DC0:0x4DFF),
    ("CJK_Unified_Ideographs", 0x4E00:0x9FFF),
    ("Yi_Syllables", 0xA000:0xA48F),
    ("Yi_Radicals", 0xA490:0xA4CF),
    ("Hangul_Syllables", 0xAC00:0xD7AF),
    ("High_Surrogates", 0xD800:0xDB7F),
    ("High_Private_Use_Surrogates", 0xDB80:0xDBFF),
    ("Low_Surrogates", 0xDC00:0xDFFF),
    ("Private_Use_Area", 0xE000:0xF8FF),
    ("CJK_Compatibility_Ideographs", 0xF900:0xFAFF),
    ("Alphabetic_Presentation_Forms", 0xFB00:0xFB4F),
    ("Arabic_Presentation_Forms-A", 0xFB50:0xFDFF),
    ("Variation_Selectors", 0xFE00:0xFE0F),
    ("Combining_Half_Marks", 0xFE20:0xFE2F),
    ("CJK_Compatibility_Forms", 0xFE30:0xFE4F),
    ("Small_Form_Variants", 0xFE50:0xFE6F),
    ("Arabic_Presentation_Forms-B", 0xFE70:0xFEFF),
    ("Halfwidth_and_Fullwidth_Forms", 0xFF00:0xFFEF),
    ("Specials", 0xFFF0:0xFFFF)
]

unicode_classes = Dict(
    x[1] => x[2:4] for x in
    [
        (:L, "Letter", "any kind of letter from any language.", Base.Unicode.UTF8PROC_CATEGORY_LU:Base.Unicode.UTF8PROC_CATEGORY_LO),
        (:Ll, "Lowercase_Letter", "a lowercase letter that has an uppercase variant.", Base.Unicode.UTF8PROC_CATEGORY_LL),
        (:Lu, "Uppercase_Letter", "an uppercase letter that has a lowercase variant.", Base.Unicode.UTF8PROC_CATEGORY_LU),
        (:Lt, "Titlecase_Letter", "a letter that appears at the start of a word when only the first letter of the word is capitalized.", Base.Unicode.UTF8PROC_CATEGORY_LT),
        (Symbol("L&"), "Cased_Letter", "a letter that exists in lowercase and uppercase variants (combination of Ll, Lu and Lt).", Base.Unicode.UTF8PROC_CATEGORY_LL:Base.Unicode.UTF8PROC_CATEGORY_LT),
        (:Lm, "Modifier_Letter", "a special character that is used like a letter.", Base.Unicode.UTF8PROC_CATEGORY_LM),
        (:Lo, "Other_Letter", "a letter or ideograph that does not have lowercase and uppercase variants.", Base.Unicode.UTF8PROC_CATEGORY_LO),
        (:M, "Mark", "a character intended to be combined with another character (e.g. accents, umlauts, enclosing boxes, etc.).", Base.Unicode.UTF8PROC_CATEGORY_MN:Base.Unicode.UTF8PROC_CATEGORY_ME),
        (:Mn, "Non_Spacing_Mark", "a character intended to be combined with another character without taking up extra space (e.g. accents, umlauts, etc.).", Base.Unicode.UTF8PROC_CATEGORY_MN),
        (:Mc, "Spacing_Combining_Mark", "a character intended to be combined with another character that takes up extra space (vowel signs in many Eastern languages).", Base.Unicode.UTF8PROC_CATEGORY_MC),
        (:Me, "Enclosing_Mark", "a character that encloses the character it is combined with (circle, square, keycap, etc.).", Base.Unicode.UTF8PROC_CATEGORY_ME),
        (:Z, "Separator", "any kind of whitespace or invisible separator.", Base.Unicode.UTF8PROC_CATEGORY_ZS:Base.Unicode.UTF8PROC_CATEGORY_ZP),
        (:Zs, "Space_Separator", "a whitespace character that is invisible, but does take up space.", Base.Unicode.UTF8PROC_CATEGORY_ZS),
        (:Zl, "Line_Separator", "line separator character U+2028.", Base.Unicode.UTF8PROC_CATEGORY_ZL),
        (:Zp, "Paragraph_Separator", "paragraph separator character U+2029.", Base.Unicode.UTF8PROC_CATEGORY_ZP),
        (:S, "Symbol", "math symbols, currency signs, dingbats, box-drawing characters, etc.", Base.Unicode.UTF8PROC_CATEGORY_SM:Base.Unicode.UTF8PROC_CATEGORY_SO),
        (:Sm, "Math_Symbol", "any mathematical symbol.", Base.Unicode.UTF8PROC_CATEGORY_SM),
        (:Sc, "Currency_Symbol", "any currency sign.", Base.Unicode.UTF8PROC_CATEGORY_SC),
        (:Sk, "Modifier_Symbol", "a combining character (mark) as a full character on its own.", Base.Unicode.UTF8PROC_CATEGORY_SK),
        (:So, "Other_Symbol", "various symbols that are not math symbols, currency signs, or combining characters.", Base.Unicode.UTF8PROC_CATEGORY_SO),
        (:N, "Number", "any kind of numeric character in any script.", Base.Unicode.UTF8PROC_CATEGORY_ND:Base.Unicode.UTF8PROC_CATEGORY_NO),
        (:Nd, "Decimal_Digit_Number", "a digit zero through nine in any script except ideographic scripts.", Base.Unicode.UTF8PROC_CATEGORY_ND),
        (:Nl, "Letter_Number", "a number that looks like a letter, such as a Roman numeral.", Base.Unicode.UTF8PROC_CATEGORY_NL),
        (:No, "Other_Number", "a superscript or subscript digit, or a number that is not a digit 0–9 (excluding numbers from ideographic scripts).", Base.Unicode.UTF8PROC_CATEGORY_NO),
        (:P, "Punctuation", "any kind of punctuation character.", Base.Unicode.UTF8PROC_CATEGORY_PC:Base.Unicode.UTF8PROC_CATEGORY_PO),
        (:Pc, "Connector_Punctuation", "a punctuation character such as an underscore that connects words.", Base.Unicode.UTF8PROC_CATEGORY_PC),
        (:Pd, "Dash_Punctuation", "any kind of hyphen or dash.", Base.Unicode.UTF8PROC_CATEGORY_PD),
        (:Ps, "Open_Punctuation", "any kind of opening bracket.", Base.Unicode.UTF8PROC_CATEGORY_PS),
        (:Pe, "Close_Punctuation", "any kind of closing bracket.", Base.Unicode.UTF8PROC_CATEGORY_PE),
        (:Pi, "Initial_Punctuation", "any kind of opening quote.", Base.Unicode.UTF8PROC_CATEGORY_PI),
        (:Pf, "Final_Punctuation", "any kind of closing quote.", Base.Unicode.UTF8PROC_CATEGORY_PF),
        (:Po, "Other_Punctuation", "any kind of punctuation character that is not a dash, bracket, quote or connector.", Base.Unicode.UTF8PROC_CATEGORY_PO),
        (:C, "Other", "invisible control characters and unused code points.", Base.Unicode.UTF8PROC_CATEGORY_CC:Base.Unicode.UTF8PROC_CATEGORY_CO),
        (:Cc, "Control", "an ASCII or Latin-1 control character: 0x00–0x1F and 0x7F–0x9F.", Base.Unicode.UTF8PROC_CATEGORY_CC),
        (:Cf, "Format", "invisible formatting indicator.", Base.Unicode.UTF8PROC_CATEGORY_CF),
        (:Cs, "Surrogate", "one half of a surrogate pair in UTF-16 encoding.", Base.Unicode.UTF8PROC_CATEGORY_CS),
        (:Co, "Private_Use", "any code point reserved for private use.", Base.Unicode.UTF8PROC_CATEGORY_CO),
        (:Cn, "Unassigned", "any code point to which no character has been assigned.", Base.Unicode.UTF8PROC_CATEGORY_CN)
    ]
)


function in_any(x,sets)
    for s in sets
        x in s && return true
    end
    return false
end

export UnicodeClass
struct UnicodeClass{I}
    class::I
end
UnicodeClass(abbrev::Symbol...) =
    UnicodeClass(tuple((unicode_classes[a][3] for a in abbrev)...))
UnicodeClass(abbrev::String...) =
    UnicodeClass((Symbol(a) for a in abbrev)...)
_ismatch(x::Char, set::UnicodeClass{<:Tuple}) =
    in_any(Base.Unicode.category_code(x),set.class)


function TextParse.tryparsenext(tok::UnicodeClass, str, i, till, opts=TextParse.default_opts)
    if i <= till
        match_unicode_class(str[i],tok.class) && (Nullable(str[i]), nextind(str,i))
    end
    Nullable{Char}(), i    
end
regex_string_(x::UnicodeClass) =
    join(["\\p{$s}" for s in x.class])

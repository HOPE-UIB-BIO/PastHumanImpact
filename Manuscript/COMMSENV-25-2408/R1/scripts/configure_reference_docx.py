"""Apply the submitted-manuscript visual system to the Quarto reference DOCX."""

from __future__ import annotations

import os
import tempfile
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET


ARTICLE_DIR = Path(__file__).resolve().parents[1]
REFERENCE_DOCX = ARTICLE_DIR / "reference" / "submitted-style-reference.docx"
W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
R_NS = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
REL_NS = "http://schemas.openxmlformats.org/package/2006/relationships"
CT_NS = "http://schemas.openxmlformats.org/package/2006/content-types"
W = f"{{{W_NS}}}"
R = f"{{{R_NS}}}"
REL = f"{{{REL_NS}}}"
CT = f"{{{CT_NS}}}"
BODY_FONT = "Times New Roman"
DISPLAY_FONT = "Aptos Display"
TEAL = "0B5873"

ET.register_namespace("w", W_NS)
ET.register_namespace("r", R_NS)
ET.register_namespace("", REL_NS)


def child(parent, tag):
    node = parent.find(W + tag)
    if node is None:
        node = ET.SubElement(parent, W + tag)
    return node


def replace_child(parent, tag, attributes):
    for existing in list(parent.findall(W + tag)):
        parent.remove(existing)
    node = ET.SubElement(parent, W + tag)
    for key, value in attributes.items():
        node.set(W + key, str(value))
    return node


def style_by_name(root, names):
    wanted = {name.casefold() for name in names}
    for style in root.findall(W + "style"):
        style_id = style.get(W + "styleId", "").casefold()
        name = style.find(W + "name")
        display_name = name.get(W + "val", "").casefold() if name is not None else ""
        if style_id in wanted or display_name in wanted:
            return style
    return None


def configure_style(root, names, font, size, color, bold, before, after, line, justify, keep_next=False, create_if_missing=True):
    style = style_by_name(root, names)
    if style is None and create_if_missing:
        style = ET.SubElement(root, W + "style")
        style.set(W + "type", "paragraph")
        style.set(W + "styleId", names[0])
        replace_child(style, "name", {"val": names[0]})
        replace_child(style, "basedOn", {"val": "Normal"})
        replace_child(style, "next", {"val": "Normal"})
    if style is None:
        return
    rpr = child(style, "rPr")
    replace_child(rpr, "rFonts", {"ascii": font, "hAnsi": font, "eastAsia": font, "cs": font})
    replace_child(rpr, "sz", {"val": round(size * 2)})
    replace_child(rpr, "szCs", {"val": round(size * 2)})
    replace_child(rpr, "color", {"val": color})
    for existing in list(rpr.findall(W + "b")) + list(rpr.findall(W + "bCs")):
        rpr.remove(existing)
    if bold:
        ET.SubElement(rpr, W + "b")
        ET.SubElement(rpr, W + "bCs")
    ppr = child(style, "pPr")
    replace_child(ppr, "spacing", {"before": before * 20, "after": after * 20, "line": round(line * 240), "lineRule": "auto"})
    replace_child(ppr, "jc", {"val": justify})
    for existing in list(ppr.findall(W + "keepNext")):
        ppr.remove(existing)
    if keep_next:
        ET.SubElement(ppr, W + "keepNext")


def configure_styles(xml_bytes):
    root = ET.fromstring(xml_bytes)
    doc_defaults = child(root, "docDefaults")
    run_default = child(child(doc_defaults, "rPrDefault"), "rPr")
    replace_child(run_default, "rFonts", {"ascii": BODY_FONT, "hAnsi": BODY_FONT, "eastAsia": BODY_FONT, "cs": BODY_FONT})
    replace_child(run_default, "sz", {"val": 24})
    replace_child(run_default, "szCs", {"val": 24})
    paragraph_default = child(child(doc_defaults, "pPrDefault"), "pPr")
    replace_child(paragraph_default, "spacing", {"before": 0, "after": 160, "line": 360, "lineRule": "auto"})
    replace_child(paragraph_default, "jc", {"val": "both"})
    for body_style in (("Normal",), ("BodyText", "Body Text"), ("FirstParagraph", "First Paragraph")):
        configure_style(root, body_style, BODY_FONT, 12, "000000", False, 0, 8, 1.5, "both")
    configure_style(root, ("Compact",), BODY_FONT, 9, "000000", False, 0, 0, 1.0, "left")
    configure_style(root, ("Title",), DISPLAY_FONT, 28, "000000", False, 0, 18, 1.0, "left", True)
    configure_style(root, ("Heading1", "Heading 1"), DISPLAY_FONT, 20, TEAL, False, 14, 8, 1.0, "left", True)
    configure_style(root, ("Heading2", "Heading 2"), DISPLAY_FONT, 16, TEAL, False, 12, 6, 1.0, "left", True)
    configure_style(root, ("Heading3", "Heading 3"), BODY_FONT, 12, "000000", True, 10, 4, 1.0, "left", True)
    for caption_style in (("Caption",), ("ImageCaption", "Image Caption"), ("TableCaption", "Table Caption")):
        configure_style(root, caption_style, BODY_FONT, 10, "000000", False, 4, 8, 1.1, "both")
    for bibliography_style in (("Bibliography",), ("References",)):
        configure_style(root, bibliography_style, BODY_FONT, 10, "000000", False, 0, 4, 1.0, "left")
    for table_style in (("TableNormal", "Table Normal"), ("TableGrid", "Table Grid")):
        configure_style(root, table_style, BODY_FONT, 9, "000000", False, 0, 0, 1.0, "left", create_if_missing=False)
    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def configure_document(xml_bytes, footer_rid):
    root = ET.fromstring(xml_bytes)
    for sect_pr in root.iter(W + "sectPr"):
        replace_child(sect_pr, "pgSz", {"w": 11906, "h": 16838})
        replace_child(sect_pr, "pgMar", {"top": 1417, "right": 1417, "bottom": 1417, "left": 1417, "header": 709, "footer": 709, "gutter": 0})
        replace_child(sect_pr, "lnNumType", {"countBy": 1, "start": 1, "restart": "continuous", "distance": 360})
        for existing in list(sect_pr.findall(W + "footerReference")):
            sect_pr.remove(existing)
        footer_ref = ET.Element(W + "footerReference")
        footer_ref.set(W + "type", "default")
        footer_ref.set(R + "id", footer_rid)
        sect_pr.insert(0, footer_ref)
    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def configure_settings(xml_bytes):
    root = ET.fromstring(xml_bytes)
    update_fields = root.find(W + "updateFields")
    if update_fields is None:
        update_fields = ET.SubElement(root, W + "updateFields")
    update_fields.set(W + "val", "true")
    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def configure_relationships(xml_bytes):
    root = ET.fromstring(xml_bytes)
    footer_type = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer"
    footer_rel = next((rel for rel in root.findall(REL + "Relationship") if rel.get("Type") == footer_type), None)
    if footer_rel is None:
        used = {rel.get("Id") for rel in root.findall(REL + "Relationship")}
        number = 1
        while f"rId{number}" in used:
            number += 1
        footer_rel = ET.SubElement(root, REL + "Relationship")
        footer_rel.set("Id", f"rId{number}")
        footer_rel.set("Type", footer_type)
        footer_rel.set("Target", "footer1.xml")
    else:
        footer_rel.set("Target", "footer1.xml")
    return ET.tostring(root, encoding="utf-8", xml_declaration=True), footer_rel.get("Id")


def configure_content_types(xml_bytes):
    root = ET.fromstring(xml_bytes)
    part_name = "/word/footer1.xml"
    if not any(item.get("PartName") == part_name for item in root.findall(CT + "Override")):
        override = ET.SubElement(root, CT + "Override")
        override.set("PartName", part_name)
        override.set("ContentType", "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml")
    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def footer_xml():
    root = ET.Element(W + "ftr")
    paragraph = ET.SubElement(root, W + "p")
    ppr = ET.SubElement(paragraph, W + "pPr")
    replace_child(ppr, "jc", {"val": "center"})
    run = ET.SubElement(paragraph, W + "r")
    begin = ET.SubElement(run, W + "fldChar")
    begin.set(W + "fldCharType", "begin")
    instruction = ET.SubElement(run, W + "instrText")
    instruction.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    instruction.text = " PAGE "
    separate = ET.SubElement(run, W + "fldChar")
    separate.set(W + "fldCharType", "separate")
    text = ET.SubElement(run, W + "t")
    text.text = "1"
    end = ET.SubElement(run, W + "fldChar")
    end.set(W + "fldCharType", "end")
    return ET.tostring(root, encoding="utf-8", xml_declaration=True)


def main():
    with zipfile.ZipFile(REFERENCE_DOCX) as source:
        parts = {name: source.read(name) for name in source.namelist()}
    relationships, footer_rid = configure_relationships(parts["word/_rels/document.xml.rels"])
    parts["word/_rels/document.xml.rels"] = relationships
    parts["word/styles.xml"] = configure_styles(parts["word/styles.xml"])
    parts["word/document.xml"] = configure_document(parts["word/document.xml"], footer_rid)
    parts["word/settings.xml"] = configure_settings(parts["word/settings.xml"])
    parts["[Content_Types].xml"] = configure_content_types(parts["[Content_Types].xml"])
    parts["word/footer1.xml"] = footer_xml()
    descriptor, temporary_name = tempfile.mkstemp(suffix=".docx", dir=REFERENCE_DOCX.parent)
    os.close(descriptor)
    temporary = Path(temporary_name)
    try:
        with zipfile.ZipFile(temporary, "w", zipfile.ZIP_DEFLATED) as target:
            for name, data in parts.items():
                target.writestr(name, data)
        os.replace(temporary, REFERENCE_DOCX)
    finally:
        temporary.unlink(missing_ok=True)


if __name__ == "__main__":
    main()

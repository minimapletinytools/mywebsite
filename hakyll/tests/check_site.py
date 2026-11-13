#!/usr/bin/env python3
"""Sanity checks for the generated site. Run after `site build`:

    python3 tests/check_site.py            (from the hakyll/ directory)

Exits non-zero and lists every problem found.
"""
import os
import re
import struct
import sys
import urllib.parse
import xml.etree.ElementTree as ET
from html.parser import HTMLParser

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SITE = os.path.join(ROOT, "_site")
IMAGE_EXTS = {".gif", ".png", ".jpg", ".jpeg", ".bmp", ".webp"}
RESIZED_EXTS = {".jpg", ".jpeg", ".png"}

errors = []


def fail(msg):
    errors.append(msg)


class Tags(HTMLParser):
    """Collects (tag, attrs) for every start tag, and the imgs inside #gallery."""

    def __init__(self):
        super().__init__()
        self.tags = []
        self.gallery_imgs = []
        self._gallery_depth = 0

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        self.tags.append((tag, attrs))
        if tag == "div":
            if self._gallery_depth:
                self._gallery_depth += 1
            elif attrs.get("id") == "gallery":
                self._gallery_depth = 1
        if tag == "img" and self._gallery_depth:
            self.gallery_imgs.append(attrs.get("src", ""))

    handle_startendtag = handle_starttag

    def handle_endtag(self, tag):
        if tag == "div" and self._gallery_depth:
            self._gallery_depth -= 1


def parse(path):
    p = Tags()
    with open(path, encoding="utf-8") as f:
        p.feed(f.read())
    return p


def front_matter(path):
    with open(path, encoding="utf-8") as f:
        text = f.read()
    m = re.match(r"---\n(.*?)\n---", text, re.S)
    meta = {}
    if m:
        for line in m.group(1).splitlines():
            if ":" in line:
                k, v = line.split(":", 1)
                meta[k.strip()] = v.strip()
    return meta


def resolve(page, url):
    """Local file a relative url on `page` points to (None for external urls)."""
    url = urllib.parse.unquote(url.split("#")[0].split("?")[0])
    if not url or re.match(r"^[a-z]+:|^//", url):
        return None
    if url.startswith("/"):
        return os.path.join(SITE, url.lstrip("/"))
    return os.path.normpath(os.path.join(os.path.dirname(page), url))


def exists_exact_case(path):
    """os.path.exists, but case-sensitive even on macOS (GitHub Pages is)."""
    path = os.path.normpath(path)
    if not os.path.exists(path):
        return False
    rel = os.path.relpath(path, SITE)
    cur = SITE
    for part in rel.split(os.sep):
        if part in ("", "."):
            continue
        if part not in os.listdir(cur):
            return False
        cur = os.path.join(cur, part)
    return True


def image_size(path):
    """(width, height) of a PNG or JPEG, without third party libraries."""
    with open(path, "rb") as f:
        data = f.read()
    if data[:8] == b"\x89PNG\r\n\x1a\n":
        return struct.unpack(">II", data[16:24])
    if data[:2] == b"\xff\xd8":
        i = 2
        while i < len(data):
            while data[i] == 0xFF:
                i += 1
            marker = data[i]
            i += 1
            if marker in (0xD8, 0x01) or 0xD0 <= marker <= 0xD7:
                continue
            (length,) = struct.unpack(">H", data[i:i + 2])
            if 0xC0 <= marker <= 0xCF and marker not in (0xC4, 0xC8, 0xCC):
                h, w = struct.unpack(">HH", data[i + 3:i + 7])
                return w, h
            i += length
    return None


def html_pages():
    for d, _, files in os.walk(SITE):
        for f in files:
            if f.endswith(".html"):
                yield os.path.join(d, f)


# ---------------------------------------------------------------------------

def check_expected_pages():
    for p in ["index.html", "about.html", "cv.html", "archive.html",
              "microblog.html", "404.html", "rss.xml"]:
        if not os.path.isfile(os.path.join(SITE, p)):
            fail(f"missing page: {p}")
    for d, _, files in os.walk(os.path.join(ROOT, "posts")):
        for f in files:
            if f.endswith(".markdown"):
                src = os.path.relpath(os.path.join(d, f), ROOT)
                out = os.path.join(SITE, src[:-len(".markdown")] + ".html")
                if not os.path.isfile(out):
                    fail(f"post not built: {src}")


def check_pages():
    for page in html_pages():
        rel = os.path.relpath(page, SITE)
        p = parse(page)
        for tag, attrs in p.tags:
            for attr in ("src", "href"):
                url = attrs.get(attr)
                if url is None:
                    continue
                # http subresources are blocked on an https site
                if attr == "src" and url.startswith("http://"):
                    fail(f"{rel}: insecure <{tag} src={url}>")
                # every local file a page references must exist
                target = resolve(page, url)
                if target and not exists_exact_case(target):
                    fail(f"{rel}: <{tag} {attr}={url}> points to a missing file"
                         " (filenames are case-sensitive)")
            if tag == "script" and "jquery" in attrs.get("src", "").lower():
                fail(f"{rel}: still loads jQuery")
        with open(page, encoding="utf-8") as f:
            text = f.read()
        if "AutoGallery" in text or "google-analytics" in text:
            fail(f"{rel}: contains old AutoGallery/analytics script")


def check_galleries():
    for d, _, files in os.walk(os.path.join(ROOT, "posts")):
        for f in files:
            if not f.endswith(".markdown"):
                continue
            src = os.path.join(d, f)
            words = front_matter(src).get("gallery", "").split()
            if len(words) < 2 or words[0] != "auto":
                continue
            rel = os.path.relpath(src, ROOT)
            folder = os.path.join(ROOT, *[x for x in words[1].split("/")
                                          if x not in ("..", ".", "")])
            expected = sorted(x for x in os.listdir(folder)
                              if os.path.splitext(x)[1].lower() in IMAGE_EXTS)
            page = os.path.join(SITE, os.path.relpath(src, ROOT)[:-len(".markdown")] + ".html")
            got = parse(page).gallery_imgs
            names = sorted(os.path.basename(urllib.parse.unquote(u)) for u in got)
            if not expected:
                fail(f"{rel}: auto gallery folder {words[1]} has no images")
            if names != expected:
                fail(f"{rel}: gallery shows {len(names)} images, folder has {len(expected)}")
            for u in got:
                ext = os.path.splitext(u)[1].lower()
                if ext in RESIZED_EXTS and "/gallery-500/" not in u:
                    fail(f"{rel}: gallery image {u} is not the 500px copy")


def check_images():
    for d, _, files in os.walk(SITE):
        for f in files:
            path = os.path.join(d, f)
            rel = os.path.relpath(path, SITE)
            ext = os.path.splitext(f)[1].lower()
            if ext not in RESIZED_EXTS:
                continue
            with open(path, "rb") as fh:
                head = fh.read(1 << 16)
            # EXIF (which holds GPS location) must be stripped
            if b"Exif\x00\x00" in head:
                fail(f"{rel}: still has EXIF metadata")
            size = image_size(path)
            if size is None:
                fail(f"{rel}: could not read image size")
                continue
            w, h = size
            limit = (500, None) if rel.startswith("gallery-500/") else (1600, 1600)
            if w > limit[0] or (limit[1] and h > limit[1]):
                fail(f"{rel}: {w}x{h} is larger than {limit}")


def check_404():
    p = parse(os.path.join(SITE, "404.html"))
    for tag, attrs in p.tags:
        for attr in ("src", "href"):
            url = attrs.get(attr, "")
            if url.startswith("."):
                fail(f"404.html: relative url {url} breaks on nested paths")


def check_rss():
    try:
        root = ET.parse(os.path.join(SITE, "rss.xml")).getroot()
        if not root.findall("./channel/item"):
            fail("rss.xml has no items")
    except ET.ParseError as e:
        fail(f"rss.xml is not valid XML: {e}")


def main():
    if not os.path.isdir(SITE):
        print("no _site directory, run `stack exec -- site build` first")
        return 1
    for check in [check_expected_pages, check_pages, check_galleries,
                  check_images, check_404, check_rss]:
        before = len(errors)
        check()
        print(f"{check.__name__}: {'ok' if len(errors) == before else 'FAILED'}")
    for e in errors:
        print("  " + e)
    print(f"{len(errors)} problem(s)")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())

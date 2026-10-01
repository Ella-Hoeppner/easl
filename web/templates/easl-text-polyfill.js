// Audio worklets have no TextDecoder/TextEncoder, which the runtime's
// generated bindings need to pass strings between JavaScript and
// WebAssembly. UTF-8 is all they use. Imported by the worklet before the
// bindings, which look for these when they load.

if (typeof globalThis.TextDecoder === "undefined") {
  globalThis.TextDecoder = class TextDecoder {
    decode(bytes) {
      if (!bytes) return "";
      const b = bytes instanceof Uint8Array ? bytes : new Uint8Array(bytes);
      let out = "";
      for (let i = 0; i < b.length; ) {
        const c = b[i++];
        let code;
        if (c < 0x80) code = c;
        else if (c < 0xe0) code = ((c & 0x1f) << 6) | (b[i++] & 0x3f);
        else if (c < 0xf0)
          code = ((c & 0x0f) << 12) | ((b[i++] & 0x3f) << 6) | (b[i++] & 0x3f);
        else
          code =
            ((c & 0x07) << 18) |
            ((b[i++] & 0x3f) << 12) |
            ((b[i++] & 0x3f) << 6) |
            (b[i++] & 0x3f);
        out += String.fromCodePoint(code);
      }
      return out;
    }
  };
}

if (typeof globalThis.TextEncoder === "undefined") {
  globalThis.TextEncoder = class TextEncoder {
    encode(text) {
      const bytes = [];
      for (const ch of text) {
        const code = ch.codePointAt(0);
        if (code < 0x80) bytes.push(code);
        else if (code < 0x800) bytes.push(0xc0 | (code >> 6), 0x80 | (code & 0x3f));
        else if (code < 0x10000)
          bytes.push(
            0xe0 | (code >> 12),
            0x80 | ((code >> 6) & 0x3f),
            0x80 | (code & 0x3f),
          );
        else
          bytes.push(
            0xf0 | (code >> 18),
            0x80 | ((code >> 12) & 0x3f),
            0x80 | ((code >> 6) & 0x3f),
            0x80 | (code & 0x3f),
          );
      }
      return new Uint8Array(bytes);
    }
    encodeInto(text, view) {
      const bytes = this.encode(text);
      const written = Math.min(bytes.length, view.length);
      view.set(bytes.subarray(0, written));
      return { read: text.length, written };
    }
  };
}

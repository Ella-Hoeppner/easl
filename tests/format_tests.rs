//! Formatter tests. Most assert *properties* (idempotency, the width
//! limit, no blank line left after a line comment) rather than exact
//! golden output, so they survive tuning of the layout heuristics; a few
//! pin specific layouts the changes were made to produce.

use easl::format::format_easl_source;

const MAX_WIDTH: usize = 80;

fn fmt(source: &str) -> String {
  format_easl_source(source).expect("source should parse")
}

/// Formatting is a fixed point: re-formatting already-formatted output
/// must not change it.
fn assert_idempotent(source: &str) {
  let once = fmt(source);
  let twice = fmt(&once);
  assert_eq!(once, twice, "formatting was not idempotent");
}

/// No emitted line exceeds the width limit, except line comments — which
/// can't be broken.
fn assert_within_width(source: &str) {
  for line in fmt(source).lines() {
    if line.trim_start().starts_with(';') {
      continue;
    }
    assert!(
      line.len() <= MAX_WIDTH,
      "line exceeds {MAX_WIDTH} columns ({}): {line:?}",
      line.len()
    );
  }
}

#[test]
fn line_comment_leaves_no_blank_line() {
  let out = fmt("(defn f []\n  ; a note\n  x)");
  assert_eq!(out, "(defn f []\n  ; a note\n  x)\n");
  assert!(
    !out.contains("\n\n"),
    "a line comment should not be followed by a blank line: {out:?}"
  );
}

#[test]
fn fn_body_indents_one_past_fn() {
  // A multi-line `fn` body sits one column past `fn`, not aligned with the
  // parameter list. Here `(fn` opens at column 5, so its body lands at
  // column 7 (one past the `f`).
  let out = fmt("(run (fn [] (first-thing) (second-thing)))");
  assert_eq!(
    out, "(run (fn []\n       (first-thing)\n       (second-thing)))\n",
    "fn body should indent one past `fn`:\n{out}"
  );
}

#[test]
fn long_named_call_puts_arguments_below() {
  // A long function name (past the inline-name threshold) with a multi-line
  // lambda argument drops the whole argument onto the next line rather than
  // shifting it far right.
  let out = fmt(
    "(defn main []
       (dispatch-compute-shader (fn [] (do-a-thing) (do-another-thing))
                                (vec3u n 1u 1u)))",
  );
  assert!(
    out.contains("(dispatch-compute-shader\n"),
    "long-named call should place its arguments below the name:\n{out}"
  );
}

#[test]
fn long_annotation_drops_type_below() {
  // A `:` annotation whose type would overflow the line puts the type on the
  // next line, one column past the annotated term, instead of exceeding the
  // width limit.
  let source = "(defn sequential-phase-mod [fundamental: (Fn [] f32)
                              stages-generator: (Fn [] [PhaseModStage])]: (Fn [] f32)
                 x)";
  let out = fmt(source);
  assert!(
    out.contains("stages-generator:\n"),
    "an overflowing type annotation should drop its type below:\n{out}"
  );
  assert_within_width(source);
}

#[test]
fn short_named_call_stays_aligned() {
  // A short name with a multi-line first argument keeps the aligned layout
  // (args under the first argument), not the fully-below layout.
  let out =
    fmt("(defn f [] (clamp (really-quite-a-long-value-here) low high))");
  assert!(
    out.contains("(clamp (really-quite-a-long-value-here)\n"),
    "short-named call should keep its first argument inline:\n{out}"
  );
}

#[test]
fn deeply_nested_code_respects_width_limit() {
  assert_within_width(
    "(defn main []
       (spawn-window
         (fn []
           (dispatch-compute-shader
             (fn []
               (= (accumulation-grid (+ (.x (global-invocation-id))
                                        (* (.x render-dimensions)
                                           (.y (global-invocation-id)))))
                  (vec4f 0.)))
             (vec3u render-dimensions 1u)))))",
  );
}

#[test]
fn idempotent_on_varied_forms() {
  assert_idempotent(
    "(def TAU: f32 6.28318530718)
     (defn rand [minimum: f32 maximum: f32]: f32
       ; a comment
       (+ minimum (* (- maximum minimum) (rand))))
     (defn main []
       (let [@var x 0u]
         (spawn-window
           (fn []
             (dispatch-render-shaders vert frag 3)))))",
  );
}

#[test]
fn small_forms_stay_on_one_line() {
  assert_eq!(fmt("(var rand-state: f32)"), "(var rand-state: f32)\n");
  assert_eq!(fmt("(vec2f (cos x) (sin x))"), "(vec2f (cos x) (sin x))\n");
}

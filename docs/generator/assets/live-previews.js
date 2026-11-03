// Runs a page's live previews. Each `.live-preview` figure holds a canvas
// and names the packaged easl program to run on it (`data-module`). A
// preview starts the first time it comes near the screen, so a page's
// previews don't all compile and start at once, and it's paused whenever
// it's off screen, so only the previews in view are running.

const previews = document.querySelectorAll(".live-preview");

// Each started preview's program, once its module has loaded.
const programs = new Map();
const started = new Set();
const visible = new Set();

if (!navigator.gpu) {
  for (const preview of previews) preview.classList.add("no-webgpu");
} else {
  const observer = new IntersectionObserver(
    (entries) => {
      for (const { target: preview, isIntersecting } of entries) {
        if (isIntersecting) {
          visible.add(preview);
          if (started.has(preview)) programs.get(preview)?.resume();
          else start(preview);
        } else {
          visible.delete(preview);
          programs.get(preview)?.pause();
        }
      }
    },
    { rootMargin: "200px" },
  );
  for (const preview of previews) observer.observe(preview);
}

async function start(preview) {
  started.add(preview);
  // The runtime treats wheel events on its canvas as program input and
  // cancels their scrolling, so they're stopped before reaching it: the
  // page keeps scrolling over a preview.
  preview.addEventListener("wheel", (event) => event.stopPropagation(), {
    capture: true,
  });
  const output = preview.querySelector(".live-console");
  try {
    const module = new URL(preview.dataset.module, location.href);
    const { startEaslProgram } = await import(module);
    const program = startEaslProgram(preview.querySelector("canvas"), {
      onPrint: output ? (line) => appendLine(output, line) : undefined,
    });
    programs.set(preview, program);
    // It may have scrolled out of view while its module loaded.
    if (!visible.has(preview)) program.pause();
    await program.finished;
  } catch (error) {
    console.error(error);
    preview.classList.add("failed");
  }
}

const MAX_CONSOLE_LINES = 200;

function appendLine(output, line) {
  const lines = output.textContent ? output.textContent.split("\n") : [];
  lines.push(line);
  output.textContent = lines.slice(-MAX_CONSOLE_LINES).join("\n");
  output.scrollTop = output.scrollHeight;
}

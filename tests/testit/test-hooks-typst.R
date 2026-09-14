library(testit)

assert("Typst document hook unwraps Pandoc raw ```{=typst} blocks (#2462)", {
  old = knit_hooks$get()
  on.exit(knit_hooks$restore(old), add = TRUE)
  knit_hooks$set(hooks_typst())
  hook = knit_hooks$get('document')

  # a raw typst block is unwrapped to its content
  (hook('```{=typst}\n#table([a])\n```\n') %==% '#table([a])\n')

  # content is kept as-is when there is no raw typst block
  (hook('regular text\n') %==% 'regular text\n')

  # only raw *typst* blocks are unwrapped; other fenced code blocks are kept
  (hook('```rust\nfn main() {}\n```\n') %==% '```rust\nfn main() {}\n```\n')

  # multiple raw typst blocks are all unwrapped
  (hook('```{=typst}\n#a\n```\n\n```{=typst}\n#b\n```\n') %==% '#a\n\n#b\n')
})

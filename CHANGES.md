
# 0.3

- refactor: extract type definitions to `iostream.types`
- perf slice: improve `Slice.find_index_from`

# 0.2.2

- bugfix for iostream-camlzip (assertion failure)

# 0.2.1

- bugfix for iostream-camlzip

# 0.2

- camlzip: add buffered version of the input stream transducers
- add In_buf.skip
- add `iostream-camlzip`, depends on `iostream`
- rename Out to Out_buf, add Out
- add `Slice` type, used for buffered input
- add `iostream.unix` optional library
- split seekable into its own class
- breaking: use OO and `class type` for all types

# 0.1

initial release

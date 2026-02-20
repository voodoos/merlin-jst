/Users/ulysse/tmp/short-paths-redesign/merlin-jst/ocamlmerlin single type-enclosing -position '133:1' -index 0  -filename /Users/ulysse/tmp/oxcaml-sp/base/src/uchar_intf.ml < /Users/ulysse/tmp/oxcaml-sp/base/src/uchar_intf.ml

sig
  module type Utf = Definitions.Utf
  type t = Uchar.t
  val hash_fold_t : t Ppx_hash_lib.hash_fold @@ portable
  val hash : t -> Hash.hash_value @@ portable
  val t_of_sexp : Sexp.t -> t @@ portable
  val sexp_of_t__stack : t @ local -> Sexp.t @ local @@ portable
  val sexp_of_t : t -> Sexp.t @@ portable
  val t_sexp_grammar : t Sexplib0.Sexp_grammar.t @@ portable
  val ( >= ) : t -> t -> bool @@ portable
  val ( <= ) : t -> t -> bool @@ portable
  val ( = ) : t -> t -> bool @@ portable
  val ( > ) : t -> t -> bool @@ portable
  val ( < ) : t -> t -> bool @@ portable
  val ( <> ) : t -> t -> bool @@ portable
  val equal : t -> t -> bool @@ portable
  val compare : t -> t -> int @@ portable
  val equal__local : t @ local -> t @ local -> bool @@ portable
  val compare__local : t @ local -> t @ local -> int @@ portable
  val min : t -> t -> t @@ portable
  val max : t -> t -> t @@ portable
  val ascending : t -> t -> int @@ portable
  val descending : t -> t -> int @@ portable
  val between : t -> low:t -> high:t -> bool @@ portable
  val clamp_exn : t -> min:t -> max:t -> t @@ portable
  val clamp : t -> min:t -> max:t -> t Or_error.t @@ portable
  type comparator_witness : value mod portable
  val comparator : (t, comparator_witness) Comparator.t @@ portable
  val pp : Format.formatter -> t -> unit @@ portable
  val invariant : t Invariant.t @@ portable
  val succ : t -> t option @@ portable
  val succ_exn : t -> t @@ portable
  val pred : t -> t option @@ portable
  val pred_exn : t -> t @@ portable
  val is_char : t -> bool @@ portable
  val to_char : t -> char option @@ portable
  val to_char_exn : t -> char @@ portable
  val of_char : char -> t @@ portable
  val int_is_scalar : int -> bool @@ portable
  val of_scalar : int -> t option @@ portable
  val of_scalar_exn : int -> t @@ portable
  val to_scalar : t -> int @@ portable
  val utf_8_byte_length : t -> int @@ portable
  val utf_16_byte_length : t -> int @@ portable
  val min_value : t @@ portable
  val max_value : t @@ portable
  val byte_order_mark : t @@ portable
  val replacement_char : t @@ portable
  module Decode_result :
    sig
      type t = Uchar0.utf_decode
      val compare :
        Uchar0.utf_decode
        Ppx_compare_lib__Ppx_compare_lib_intf.Definitions.compare @@ portable
      val compare__local :
        Uchar0.utf_decode
        Ppx_compare_lib__Ppx_compare_lib_intf.Definitions.compare__local @@
        portable
      val equal :
        Uchar0.utf_decode
        Ppx_compare_lib__Ppx_compare_lib_intf.Definitions.equal @@ portable
      val equal__local :
        Uchar0.utf_decode
        Ppx_compare_lib__Ppx_compare_lib_intf.Definitions.equal__local @@
        portable
      val hash_fold_t : Uchar0.utf_decode Ppx_hash_lib.hash_fold @@ portable
      val hash : Uchar0.utf_decode -> Hash.hash_value @@ portable
      val sexp_of_t : Uchar0.utf_decode -> Sexp.t @@ portable
      val sexp_of_t__stack : Uchar0.utf_decode @ local -> Sexp.t @ local @@
        portable
      val is_valid : Uchar0.utf_decode -> bool @@ portable
      val bytes_consumed : Uchar0.utf_decode -> int @@ portable
      val uchar : Uchar0.utf_decode -> Uchar.t option @@ portable
      val uchar_exn : Uchar0.utf_decode -> Uchar.t @@ portable
      val uchar_or_replacement_char : Uchar0.utf_decode -> Uchar.t @@
        portable
    end
  module Utf8 :
    sig
      val of_string : string -> t @@ portable
      val to_string : t -> string @@ portable
      val byte_length : t -> int @@ portable
      val codec_name : string @@ portable
    end
  module Utf16le :
    sig
      val of_string : string -> t @@ portable
      val to_string : t -> string @@ portable
      val byte_length : t -> int @@ portable
      val codec_name : string @@ portable
    end
  module Utf16be :
    sig
      val of_string : string -> t @@ portable
      val to_string : t -> string @@ portable
      val byte_length : t -> int @@ portable
      val codec_name : string @@ portable
    end
  module Utf32le :
    sig
      val of_string : string -> t @@ portable
      val to_string : t -> string @@ portable
      val byte_length : t -> int @@ portable
      val codec_name : string @@ portable
    end
  module Utf32be :
    sig
      val of_string : string -> t @@ portable
      val to_string : t -> string @@ portable
      val byte_length : t -> int @@ portable
      val codec_name : string @@ portable
    end
end

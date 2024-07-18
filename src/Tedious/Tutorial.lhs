> module Tedious.Tutorial () where

[tedious|tedious_str|]

ABNF:
  tedious_str = *WSP indent_block
  indent_block = type_info 1*type_field
  type_info = type_name *type_attr [comment] end_of_line
  type_name = name_upper
  type_attr = 1*WSP (type_attr_table / type_attr_deriv)
  type_attr_table = "table" 1*WSP table_info
  table_info = table_name / "(" *WSP table_schema *WSP "," *WSP table_name *WSP ")"
  table_schema = name
  table_name = name
  type_attr_deriv = "deriving" 1*(1*WSP name_upper)
  type_field = 1*WSP field_name [field_desc] field_table *ext_type [comment] end_of_line
  field_name = name_lower
  field_desc = 1*WSP "`" *VCHAR "`"
  field_table = 1*WSP (field_type / field_type_var)
  field_type = (field_type_paren / field_type_proto) ["?"] [field_samp] [opaleye_type] ["#"] *table_unique
  field_type_paren = "(" *WSP field_type_proto *WSP ")"
  field_type_proto = field_type_array / field_type_tuple / field_type_combo
  field_type_array = "[" *WSP field_type_proto *WSP "]"
  field_type_tuple = "(" *WSP field_type_proto *field_type_tuple_part *WSP ")"
  field_type_tuple_part = *WSP "," *WSP field_type_proto 
  field_type_combo = field_type_plain *(1*WSP field_type_proto)
  field_type_plain = name_upper *(1*WSP name_upper)
  field_type_var = name_lower ["?"]
  field_samp = 1*WSP "`" *VCHAR "`"
  opaleye_type = 1*WSP "(" *WSP opaleye_r / opaleye_nr / opaleye_wr / opaleye_nwr *WSP ")"
  opaleye_r = field_type_combo
  opaleye_nr = DQUOTE *VCHAR DQUOTE *WSP "," *WSP field_type_combo
  opaleye_wr = field_type_combo *WSP "," *WSP field_type_combo
  opaleye_nwr = DQUOTE *VCHAR DQUOTE *WSP "," *WSP field_type_combo *WSP "," *WSP field_type_combo
  table_unique = 1*WSP "!" name
  ext_type = 1*WSP name_upper [ext_type_var]
  ext_type_var = ":" name_lower
  char_name = ALPHA / DIGIT / "_" / "'" 
  alpha_upper = A-Z
  alpha_lower = a-z
  name = 1*char_name
  name_upper = alpha_upper 1*char_name
  name_lower = alpha_lower 1*char_name
  comment = "-" "-" *VCHAR
  end_of_line = *WSP 1*CRLF



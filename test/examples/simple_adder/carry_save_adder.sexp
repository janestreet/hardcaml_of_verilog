((
  top (
    (module_name carry_save_adder)
    (path        simple_adder.v)
    (instantiates ((
      (module_name fa)
      (path        full_adder.v)
      (instantiates ((
        (module_name ha)
        (path        full_adder.v))))))))))

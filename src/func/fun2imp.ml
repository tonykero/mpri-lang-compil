let translate_program prog =
    let clj = Fun2clj.translate_program prog in
    Clj2imp.translate_program clj
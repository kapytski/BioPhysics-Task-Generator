tasks<-list(
  task1=paste0("Тело излучило количество энергии равное \\(E\\=##slot_1##\\;мДж\\) за время ",
               "\\(\\Delta t\\=##slot_2##\\;с\\). Тело представляет собой куб ",
               " с длиной ребра \\(a\\=##slot_3##\\;см\\). Чему равна энергетическая светимость",
               " \\(R\\)?"),
  task2=paste0("Найти количество энергии \\(E\\), излучаемой за одну секунду с поверхности серого тела с постоянным монохроматическим показателем",
               "  поглощения \\(\\alpha\\=##slot_1##\\). Тело имеет форму сферы радиуса",
               " \\(r\\=##slot_2##\\;см\\) и температуру поверхности \\(T\\=##slot_3##\\;K\\)."),
  task3=paste0("In an eclosure there are 2 bodies: a real body and the blackbody. They are in thermodynamic equilibrium at",
               " the temperature \\(T\\=##slot_1##\\;K\\). Find the emissivity \\(r_\\lambda\\) of the real body",
               " at the wavelength \\(\\lambda\\=##slot_2##\\;\\mu m\\), if the absorptivity of the real body",
               " (at these temperature and wavelength) is equal to \\(\\alpha_\\lambda\\=##slot_3##\\)."),
  task4=paste0("Температура абсолютно чёрного тела повысилась с \\(T_1\\=##slot_1##\\;K\\) до",
               " \\(T_2\\=##slot_2##\\;K\\). Чему равно абсолютное значение разности \\(\\Delta \\lambda\\) длин волн \\(\\lambda_1\\)",
               " и \\(\\lambda_2\\), соответствующих максимумам спектральных плотностей энергетических светимостей",
               " при данных температурах?"),
  task5=paste0("What is the ambient temperature \\(T_1\\) if unclothed human with the skin temperature \\(T_2\\=##slot_1##\\;^\\circ C\\)",
               " releases each 1 second amount of energy \\(E\\=##slot_2##\\;J\\). Consider skin surface as the grey body",
               " with constant absorptivity equal to \\(\\alpha\\=##slot_3##\\); area of skin should be taken",
               " as \\(S\\=##slot_4##\\;m^2\\).")
)

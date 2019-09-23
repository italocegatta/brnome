library(brnome)

# frequencia --------------------------------------------------------------

library(magrittr)
data("localidades")

nome = "eliza"
sexo = "f"
localidade_cod = 33
localidade_cod = 1100023



brnome_freq("Eliza")
brnome_freq("italo", "f")
brnome_freq("italo", "f", 33)
brnome_freq("eliza", localidade_cod = 33)
brnome_freq("eliza", localidade_cod =  1100023)
brnome_freq("italo", localidade_cod =  3300100)

brnome_freq_v1("Eliza")
brnome_freq_v1("italo", "f")
brnome_freq_v1("italo", "f", 33)
brnome_freq_v1("eliza", localidade_cod = 33)
brnome_freq_v1("eliza", "f", localidade_cod = 33)
brnome_freq_v1("eliza", "m", localidade_cod = 33)
brnome_freq_v1("eliza", localidade_cod =  1100023)
brnome_freq_v1("italo", localidade_cod =  3300100)
brnome_freq_v1("eliza", "f", localidade_cod = 33, 2000)

# rank --------------------------------------------------------------------

library(magrittr)
data("localidades")
sexo = "M"
localidade_cod = 3300100
localidade_cod = 33
decada_nascimento = 2000

brnome_rank()
brnome_rank(sexo = "M")
brnome_rank(localidade_cod = 33)
brnome_rank(localidade_cod = 3300100)
brnome_rank(decada = 2000)

brnome_rank(sexo = "M", localidade_cod = 33)
brnome_rank(sexo = "M", decada = 2000, localidade_cod = 3300100)

brnome_rank_v1()
brnome_rank_v1(sexo = "M")
brnome_rank_v1(localidade_cod = 33)
brnome_rank_v1(localidade_cod = 3300100)
brnome_rank_v1(sexo = "F", decada = 2000)
brnome_rank_v1(sexo = "F", localidade_cod = 33)
brnome_rank_v1(sexo = "F", localidade_cod = 3300100)
brnome_rank_v1(sexo = "F", decada = 2000)
brnome_rank_v1(sexo = "F", decada = 2000, localidade_cod = 3300100)

library(seafile)
library(gmailr)
library(tidyverse)



clean_rename <- function(from){
  clean_vec <- function (vec, verbose = FALSE, unique = TRUE, keep_number = FALSE,
                         translit = TRUE, punct = TRUE)
  {

    old <- vec

    ext <- tools::file_ext(vec)
    vec <- tools::file_path_sans_ext(vec)

    vec <- tolower(vec)
    if (unique) {
      vec <- thinkr::make_unique(vec)
    }
    if (!keep_number) {
      vec <- make.names(vec)
    }
    if (translit) {
      vec <- stringi::stri_trans_general(vec, "latin-ascii")
    }
    if (punct) {
      vec <- vec %>% gsub(perl = TRUE, "[[:punct:]]+", "_",
                          .)
    }
    vec <- vec %>% gsub(perl = TRUE, "[[:space:]]+", "_", .) %>%
      gsub(perl = TRUE, "^_+", "", .) %>% gsub(perl = TRUE,
                                               "_+$", "", .) %>% gsub(perl = TRUE, "_+", "_", .) %>%
      tolower
    if (!keep_number) {
      vec <- make.names(vec)
    }
    if (unique) {
      vec <- thinkr::make_unique(vec)
    }
    if (verbose) {
      print(data.frame(old = old, new = vec))
    }

    vec <- paste(vec,ext,sep=".")
    invisible(vec)
  }

  out <- clean_vec(from)
  file.rename(from=from,to = out)
  out
}
#
# dd <- "a/a b/a c/"
# create_dir_r(dd)
# seafile::upload_file(path = "biscuits_a_la_pate_d_arachide_dakatine.pdf",output_directory = dd)
#


# pas de / au dedebut de dossier_upload.
dossier_upload <- glue::glue("plop/compta/justificatif frais {annee}/{dmonth}-{month}",
                             annee=format(Sys.Date(),"%Y"),
                             dmonth = format(Sys.Date(),"%m"),
                             month = locale(date_names = "fr")$date_names$mon[as.numeric(format(Sys.Date(),"%m"))]
                             ) #%>% str_replace_all(" ","+")


# on récupere les message de l'inbox

les_messages <- gmailr::messages(label_ids = "INBOX")[[1]]$messages %>%
  map_chr("id")


# on upload les fichier sur seafile
les_messages %>%
  map(message) %>%
  map(save_attachments) %>%
  compact() %>%
  unlist() %>%
  map_chr(clean_rename) -> hop

hop %>%
  map(~upload_file(path = .x,
                   output_directory = dossier_upload,
                   repos_id_destination = get_repo_id(name="Pauline")))

# on met le label seafile aux mail traité, et on les retire de l'inbox
# TODO ptet verifier que la piece jointe est sur seafile ??
les_messages %>%
  map(~modify_message(id = .x, add_labels = "Label_1",remove_labels="INBOX"))
#
# gmailr::labels()$labels %>% map_chr("name")
# gmailr::labels()
# gmailr::modify_message()
# gmailr::create_label(name = "seafile",
#                      label_list_visibility = "show",message_list_visibility = "show")
# gmailr::delete_message(id = "1674cf80f2e607dd")
# # thinkr::clean_vec("BISCUITS À LA PÂTE D'ARACHIDE DAKATINE.pdf") %>% print()

library(tidyverse)

### Read scraped channels into R ###

files <- list.files("./Data/Channels_RDS/", full.names = TRUE)

telegrams <- lapply(files, read_rds)

### Clean the data using for-loop (for each channel and nested loop for each message in channel) ###

telegram_list <- list()

for (i in 1:length(telegrams)) {

  tmp <- telegrams[[i]] %>%
    drop_na(message)
  # Equals doing this: data %>% filter(!`_` == "MessageService") %>% nrow()
  # So I guess MessageService is something other than Message which produces NAs in the message field

  message(paste0("Now working on channel no. ", i, ": ", stringr::str_to_upper(unique(tmp$source)), "."))

  ## Choosing which variables to include
  ## https://core.telegram.org/constructor/message

  ## Some variables are not present in all the channels - I have commented them out

  tmp1 <- tmp %>%
    select(source, peer_id.channel_id,
           date, id, message,
           post_author, views,
           edit_hide, edit_date,
           silent, from_scheduled, pinned,
           # reply_to._, reply_to.reply_to_peer_id, via_bot_id,
           forwards, noforwards, from_id,
           fwd_from.date, fwd_from.imported, fwd_from.channel_post, fwd_from.post_author, fwd_from.from_id.channel_id,
           media.nopremium,
           media.document._, media.document.id, media.document.mime_type, media.document.size,
           media.photo._, media.photo.id, media.photo.date,
           media.webpage._, media.webpage.id, media.webpage.url, media.webpage.type, media.webpage.site_name, media.webpage.title, media.webpage.description,
           media.webpage.photo._, media.webpage.photo.id,
           # media.webpage.document._, media.webpage.document.id, media.webpage.document.date,
           # media.poll._, media.poll.id, media.poll.question, media.poll.answers, media.poll.closed, media.poll.public_voters,
           # media.results._, media.results.results, media.results.total_voters,
           restriction_reason,
           action._)

  message("Selected the variables.")

  #### Adding nested data ####

  ## Making id variable to join by later ##

  ids <- tmp %>% pull(id)

  ## Getting reactions from nested list ##

  if(is.null(tmp$reactions.results) == FALSE){ # Not all channels have the reaction.results variable. If this exists, then...

    message("Getting reactions and putting into dataframe.")

    reactions <- tmp %>%
      select(reactions.results) %>% # Pick this variable from the dataframe
      .[[1]]

    reactionlist <- list()

    for (j in 1:length(reactions)) { # And loop over each nested dataframe in the variable

      if(length(reactions[[j]]) != 0){ # Since some messages have empty dataframes (no reaction), condition on not being 0 of length

        reactionlist[[j]] <- reactions[[j]] %>% # And pick out the ids, plus remove the unecessary "_" variable
          mutate(id = ids[j]) %>%
          select(-`_`)

      } else {

        reactionlist[[j]] <- data.frame(reaction  = "no_reaction_in_api", # Otherwise make an empty table so to be able to use do.call() with rbind() later
                                        count = NA,
                                        chosen = NA,
                                        id = ids[j])
      }

    }

    reactions <- bind_rows(reactionlist) %>% # Make the list into a dataframe
      filter(is.na(reaction)) %>%
      select(-reaction, -`reaction._`, -chosen_order)

    reactions_nest <- reactions %>%
      group_by(id) %>%
      nest(reaction = c(reaction.emoticon, count, chosen)) # And nest the reactions to each message id

    tmp_join1 <- left_join(tmp1, reactions_nest, by = "id") # Left join with the main dataset

    message("Done adding reactions.")

  } else { # If the channel does not have any reactions, make empty dataframe and nest (to use do.call() later)

    message("No reactions in this channel, making empty dataframe.")

    tmp_join1 <- tmp1 %>%
      mutate(reaction = NA,
             count = NA,
             chosen = NA) %>%
      group_by(id) %>%
      nest(reaction = c(reaction, count, chosen))

  }


  ## Getting URLs from entities in nested list ##

  message("Grabbing URLs from entities.")

  entity_ids <- tmp %>%
    select(entities) %>% # Select the "entities" variable from the dataframe - it contains URLs, among other things
    .[[1]]

  entitieslist <- list()

  for(k in 1:length(entity_ids)){

    if(length(entity_ids[[k]]) != 0){ # Some messages have empty nested dataframes. Check if this is the case.

      if(is.null(entity_ids[[k]]$url) == FALSE){ # Some messages do not have URLs. If the cell with URL information is not empty, then...

        entitieslist[[k]] <- data.frame(id = ids[k],
                                        MessageEntityTextUrl = discard(entity_ids[[k]]$url, is.na)) # Gather the URLs and discard NAs

      } else {

        entitieslist[[k]] <- data.frame(id = ids[k], # If not, make a dataframe with the same variables but empty string on URL
                                        MessageEntityTextUrl = NA)

      }

    } else { # If the message has an empty nested dataframe, make a new one and put empty string in the URL variable

      entitieslist[[k]] <- data.frame(id = ids[[k]],
                                      MessageEntityTextUrl = NA)

    }

  }

  message("Done extracting the URLs.")

  entities <- do.call(rbind, entitieslist) # Putting the entities together from list to dataframe

  entities_nest <- entities %>%
    group_by(id) %>%
    nest(entity = MessageEntityTextUrl) # Nesting to get one set of URLs per message

  telegram <- left_join(tmp_join1, entities_nest, by = "id") # Joining together with the main dataframe

  message(paste0("Added it all together, telegram channel no. ", i, " is done!"))


  ### Put the prepared dataframe for the channel into the telegram list ###

  telegram_list[[i]] <- as.data.frame(telegram)

}


telegrams_cleaned <- bind_rows(telegram_list)

telegrams_cleaned <- telegrams_cleaned %>%
  rowid_to_column() %>%
  mutate(source = str_remove_all(source, ".rds"))

saveRDS(telegrams_cleaned, file = "./Data/telegrams_cleaned.rds")


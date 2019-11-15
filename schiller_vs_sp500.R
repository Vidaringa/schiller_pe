
library(tidyverse)
library(quantmod)
library(xts)
library(lubridate)


# Næ í gögn af yahoo með quantmod -----------------------------------------

i_dag <- today() + 1

getSymbols("^GSPC",
           src = "yahoo",
           from = as.Date("1871-01-01"),
           to = as.Date(i_dag))

gspc <- as.data.frame(GSPC)
colnames(gspc) <- c("open","high","low","close","volume","adjusted")
gspc <- rownames_to_column(gspc, var = "date")
gspc$date <- ymd(gspc$date)
gspc <- gspc %>% as_tibble()

gspc <- gspc %>%
        select(date, close)

sp_value <- gspc %>%
        mutate(year = year(date),
               month = month(date)) %>%
        group_by(year, month) %>%
        slice(1) %>%
        mutate(date = floor_date(date, "month")) %>%
        ungroup() %>%
        select(date, close)



# Schiller ----------------------------------------------------------------
library(rvest)

sp_url <- "https://www.multpl.com/shiller-pe/table/by-month"

sp_pe <- sp_url %>%
        read_html() %>%
        html_nodes("td") %>%
        html_text()

sp_pe <- sp_pe %>%
        matrix(ncol = 2, byrow = TRUE) %>%
        as_tibble()

sp_pe <- sp_pe %>%
        transmute(value = parse_number(V2),
                  date = anytime::anydate(V1))

sp_pe <- sp_pe %>%
        tail(nrow(sp_pe) - 1)


# PE vs return ------------------------------------------------------------

df <- sp_pe %>% left_join(sp_value) %>%
        arrange(date) %>%
        na.omit() %>%
        select(date, value, close)

colnames(df) <- c("date", "pe_ratio", "sp_500")

ar = 10
manudir = ar * 12

df_filt <- df %>%
        mutate(sp_return = lead(sp_500, manudir)/sp_500 - 1) %>%
        na.omit() %>%
        mutate(timabil = cut(year(date),
                             breaks = seq(1920, 2020, by = 10),
                             labels = paste("Timabil:", seq(1920, 2010, by = 10))))

ggplot(df_filt,
       aes(x = pe_ratio,
           y = sp_return,
           col = timabil)) +
        geom_point() +
        geom_smooth(aes(col = timabil),
                    se = FALSE,
                    method = "lm") +
        geom_smooth(span = 0.8, method = "loess", col = "darkblue", lwd = 1.5, linetype = "dashed") +
        labs(x = "Schiller PE Ratio",
             y = paste("Ávöxtun næstu", manudir, "mánaða")) +
        geom_vline(xintercept = tail(df$pe_ratio, 1)) +
        ggthemes::scale_color_tableau() +
        theme(legend.position = "bottom")


ggplot(df_filt,
       aes(x = pe_ratio,
           y = sp_return)) +
        geom_point() +
        geom_smooth(method = "lm") +
        geom_vline(xintercept = tail(df$pe_ratio, 1), linetype = "dashed", col = "red", lwd = 1) +
        ggthemes::scale_color_tableau() +
        labs(x = "Shiller PE Ratio",
             y = "Ávöxtun næstu 60 mánaða") +
        scale_y_continuous(labels = scales::percent)



# Tímabil með hærra gildi en nú -------------------------------------------

df_high <- df %>%
        mutate(sp_return = lead(sp_500, manudir)/sp_500 - 1) %>%
        filter(pe_ratio >= tail(df$pe_ratio, 1)) %>%
        na.omit()

df_high %>%
        ggplot(aes(x = sp_return)) + geom_histogram(fill = "darkblue") + ggthemes::scale_fill_canva()


sp_list <- list()

for(i in 1:100000) {
        urtak = sample(df_high$sp_return, nrow(df_high), replace = TRUE)
        medaltal <- mean(urtak)
        sp_list[[i]] <- medaltal
}

sp_unlist <- tibble(avoxtun = unlist(sp_list))


ggplot(sp_unlist,
       aes(x = avoxtun)) +
        geom_histogram(fill = "darkblue",
                       col = "white",
                       binwidth = 0.001)

g_ecdf <- ggplot(sp_unlist,
       aes(x = avoxtun)) +
        stat_ecdf(geom = "line") +
        labs(x = "Ávöxtun",
             y = "cumulative frequency")

plotly::ggplotly(g_ecdf)



# Bayes -------------------------------------------------------------------

library(bayestestR)


interv <- ci(sp_unlist$avoxtun, ci = 0.90)

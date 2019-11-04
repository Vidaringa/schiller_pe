
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


manudir = 5 * 12

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
        geom_smooth(method = "lm", col = "black", lwd = 1.5, linetype = "dashed") +
        labs(x = "Schiller PE Ratio",
             y = paste("Ávöxtun næstu", manudir, "mánaða")) +
        geom_vline(xintercept = tail(df$pe_ratio, 1)) +
        ggthemes::scale_color_tableau()


ggplot(df_filt,
       aes(x = pe_ratio,
           y = sp_return,
           fill = timabil)) +
        geom_boxplot()


df_filt %>%
        select(date, pe_ratio, sp_return) %>%
        pivot_longer(cols = pe_ratio:sp_return,
                     names_t)

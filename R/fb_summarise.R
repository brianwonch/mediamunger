#' Calculate aggregate sums and ratios of facebook metrics
#'
#' @param df input data frame
#'
#' @return summarized data frame
#' @export
#' @importFrom magrittr "%>%"
fb_summarise = function(df){
    df %>%
        dplyr::summarise(
            reach = sum(reach, na.rm = T),
            impressions = sum(
                impressions, na.rm = T),
            amount_spent_usd = sum(
                amount_spent_usd, na.rm = T),
            page_likes_28_days_after_clicking = sum(
                page_likes_28_days_after_clicking,
                na.rm = T),
            page_likes_28_days_after_viewing = sum(
                page_likes_28_days_after_viewing,
                na.rm = T),
            post_engagement_28_days_after_clicking = sum(
                post_engagement_28_days_after_clicking,
                na.rm = T),
            post_engagement_28_days_after_viewing = sum(
                post_engagement_28_days_after_viewing,
                na.rm = T),
            post_likes_28_days_after_clicking = sum(
                post_likes_28_days_after_clicking,
                na.rm = T),
            post_likes_28_days_after_viewing = sum(
                post_likes_28_days_after_viewing,
                na.rm = T),
            post_shares_28_days_after_clicking = sum(
                post_shares_28_days_after_clicking,
                na.rm = T),
            post_shares_28_days_after_viewing = sum(
                post_shares_28_days_after_viewing,
                na.rm = T),
            website_clicks_28_days_after_clicking = sum(
                website_clicks_28_days_after_clicking,
                na.rm = T),
            website_clicks_28_days_after_viewing = sum(
                website_clicks_28_days_after_viewing,
                na.rm = T),
            offer_claims_28_days_after_clicking = sum(
                offer_claims_28_days_after_clicking,
                na.rm = T),
            offer_claims_28_days_after_viewing = sum(
                offer_claims_28_days_after_viewing,
                na.rm = T),
            page_engagement_28_days_after_clicking = sum(
                page_engagement_28_days_after_clicking,
                na.rm = T),
            page_engagement_28_days_after_viewing = sum(
                page_engagement_28_days_after_viewing,
                na.rm = T),
            checkouts_conversion_pixel__28_days_after_clicking = sum(
                checkouts_conversion_pixel__28_days_after_clicking,
                na.rm = T),
            checkouts_conversion_pixel__28_days_after_viewing = sum(
                checkouts_conversion_pixel__28_days_after_viewing,
                na.rm = T),
            post_comments_28_days_after_clicking = sum(
                post_comments_28_days_after_clicking,
                na.rm = T),
            post_comments_28_days_after_viewing = sum(
                post_comments_28_days_after_viewing,
                na.rm = T)
            ) %>%

        dplyr::mutate(
            cost = amount_spent_usd,
            frequency = reach / impressions,
            page_likes = page_likes_28_days_after_clicking +
                page_likes_28_days_after_viewing,
            post_engagement = post_engagement_28_days_after_clicking +
                post_engagement_28_days_after_viewing,
            post_likes = post_likes_28_days_after_clicking +
                post_likes_28_days_after_viewing,
            post_shares = post_shares_28_days_after_clicking +
                post_shares_28_days_after_viewing,
            post_comments = post_comments_28_days_after_clicking +
                post_comments_28_days_after_viewing,
            website_clicks = website_clicks_28_days_after_clicking +
                website_clicks_28_days_after_viewing,
            offer_claims = offer_claims_28_days_after_clicking +
                offer_claims_28_days_after_viewing,
            page_engagement = page_engagement_28_days_after_clicking +
                page_engagement_28_days_after_viewing,
            checkouts = checkouts_conversion_pixel__28_days_after_clicking +
                checkouts_conversion_pixel__28_days_after_viewing
        ) %>%

        dplyr::mutate(
            clickthrough_rate = round(website_clicks / reach, 4)*100,
            post_engagement_rate = round(post_engagement / reach, 4)*100,
            offer_claim_rate = round(offer_claims / reach, 4)*100,
            page_like_rate = round(page_likes / reach, 4)*100,
            checkout_per_reach = round(checkouts / reach, 4)*100,
            cost_per_page_like = round(cost / page_likes, 2),
            cost_per_post_engagement = round(cost / post_engagement, 2),
            cost_per_website_click = round(cost / website_clicks, 2),
            cost_per_offer_claim = round(cost / offer_claims, 2),
            cost_per_checkout = round(cost / checkouts, 2)
        )
}

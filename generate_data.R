generate_ice_cream_data <- function(
    n,
    lactose_intolerant_proportion = 0.2,
    interaction_effect_proportion = 1
) {
    scoops <- rnorm(n, 3, 1.5) |>
        round() |>
        sapply(function(x) max(0, x));

    lactose <- rnorm(n) > 0;
    gender <- rnorm(n) > 0;

    scoop_effect_size = 20;
    interaction_effect_size = -(scoop_effect_size * interaction_effect_proportion);

    happiness <- (
        (scoop_effect_size * scoops) + (interaction_effect_size * scoops * lactose) + -(3 * lactose) + (gender * -10)
    ) |>
        jitter(factor = 30) |> 
        sapply(function(x) max(0, min(x, 100))) |>
        round();

    ice_cream <- data.frame(
        Ice_Cream_Scoops = scoops,
        Happiness = happiness,
        Lactose_Intolerant = lactose,
        Gender = c("M", "F")[gender + 1]
    );

    return(ice_cream);
}

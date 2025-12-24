# AI Diffusion and Productivity Divergence in Developing Economies

**Senior Honours Thesis, University of Waterloo (2025)**  
**Supervised by:** Professor Horatiu Rus

## Abstract

This paper investigates whether AI technology promotes economic convergence or exacerbates inequality through international trade channels. Using a novel concordance linking bilateral trade data to AI exposure metrics, I analyze panel data across 47 countries from 2012-2022. 

**Key Findings:**
- No evidence of leapfrogging: developing economies show no higher marginal returns to AI-embodied diffusion
- Human capital acts as binding constraint on productivity gains from AI imports
- AI-embodied technology diffusion operates as a force for divergence rather than convergence

## Methodology

### Novel Concordance Construction
Created first-of-its-kind mapping between:
- UN Comtrade bilateral trade data (CPC classification)
- AI Industry Exposure (AIIE) estimates (Felten et al. 2021)
- Multi-step concordance: CPC → ISIC Rev. 4 → NAICS 2017 → AIIE scores

### Empirical Strategy
- **Sample:** Balanced panel of 47 countries (16 LIC, 13 LMIC, 18 HIC), 2012-2022
- **Measure:** AI-embodied imports per worker from US & China (AI frontier)
- **Analysis:** Fixed effects regressions testing leapfrogging vs. absorptive capacity hypotheses

## Key Results

**GDP Growth Effects:**
- No significant effect of AI-embodied imports on GDP growth for either developed or developing economies
- Consistent with GPT productivity paradox during early adoption phase

**TFP Growth & Absorptive Capacity:**
- Positive interaction between AI imports and human capital: countries with higher education levels derive greater productivity gains
- Negative main effect for countries at mean human capital levels
- Evidence that human capital is binding constraint on translating AI imports into productivity

## Data Sources

- **Trade Data:** UN Comtrade (CPC Ver 2.1)
- **AI Exposure:** Felten, Raj & Seamans (2021) AIIE estimates
- **Macro Variables:** Penn World Table 11.0, World Bank WDI

## Files

- `AI_Diffusion_and_Productivity_Divergence_Nour_Elkhawass.pdf` - Full thesis paper
- `final_regressions_code.R` - Main empirical analysis
- `master_concordance_final.csv` - Novel CPC-to-AIIE concordance table

## Citation
```bibtex
@thesis{elkhawass2025,
  author = {Elkhawass, Nour},
  title = {AI Diffusion and Productivity Divergence in Developing Economies},
  school = {University of Waterloo},
  year = {2025},
  type = {Undergraduate Honours Thesis}
}
```

## Contact

Nour Elkhawass  
Email: nelkhawa@uwaterloo.ca  
[LinkedIn](https://linkedin.com/in/nour-elkhawass) | [Website](https://nourelkhawass.com)

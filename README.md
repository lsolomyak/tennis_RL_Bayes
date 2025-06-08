# Bayesian Reinforcement Learning Analysis of Professional Tennis

## Abstract

Despite emotions' ubiquitous influence on learning and decision-making, studying these processes together has remained challenging, largely because emotions are considered internal experiences inaccessible to researchers. Recent work suggests that two classes of emotions map onto distinct reinforcement learning computations, providing a framework for studying emotional processes through observable behavior. 

In this framework, the key factor arbitrating between the two classes of emotions and computations is environmental controllability. In controllable environments, emotions evaluate actions, driving increased investment following negative outcomes to adjust policy. In uncontrollable environments, emotions track reward availability, with negative outcomes deflating reward-seeking motivation. 

After testing these predictions in a controlled experiment, we apply this framework to analyze professional tennis matches (N=6,715), which involve both intense emotional states and performance fluctuations. In line with the framework's predictions, players' responses to prediction errors depended on controllability: in game situations where performance changes have greater impact on winning, players increase the speed of their serves and their level of play following negative prediction errors, whereas in low controllability situations, these improvements follow positive predictions errors. 

These findings demonstrate how integrating emotions in reinforcement learning helps explain real-world learning and decision-making.

## 🎾 Research Overview

This repository implements **Bayesian reinforcement learning models** to analyze how environmental controllability affects player behavior in professional tennis. The analysis uses **hierarchical Bayesian models implemented in Stan** to capture player-specific learning patterns and emotional responses to prediction errors.

### Key Contributions
- **Bayesian RL Framework**: Novel integration of emotions into reinforcement learning through controllability-dependent learning rules
- **Stan Implementation**: Hierarchical models capturing individual player differences and contextual effects  
- **Real-world Application**: Analysis of 6,715 professional tennis matches demonstrating theoretical predictions
- **Controllability Analysis**: Quantification of how situational control affects learning and performance adjustments

## 🛠️ Technical Implementation

**Core Technologies:**
- **Stan**: Bayesian model specification and MCMC sampling
- **Python**: Data processing, model fitting, and analysis pipeline
- **Hierarchical Modeling**: Player-specific random effects and contextual variables

## 📁 Repository Structure

```
├── data/                     # Tennis match datasets (processed data excluded from repo)
├── scripts/                  # Python analysis pipeline
├── stan_models/             # Bayesian model specifications
│   ├── base_model.stan
│   ├── surprise_serve_lr.stan
│   ├── control_uniform.stan
│   └── surprise_serve_lr_progress_*.stan  # Model variants
├── stan_results/            # Model outputs and diagnostics  
├── visualizations/          # Analysis plots and figures
└── README.md
```

## 🚀 Getting Started

### Prerequisites
```bash
# Python dependencies
pip install pystan pandas numpy matplotlib seaborn jupyter

# For Stan model compilation
pip install cmdstanpy
```

### Running the Analysis
```bash
# 1. Clone repository
git clone https://github.com/lsolomyak/tennis_RL_Bayes.git
cd tennis_RL_Bayes

# 2. Run Bayesian analysis pipeline
python scripts/run_analysis.py

# 3. Generate model diagnostics
python scripts/model_diagnostics.py
```

## 🔬 Bayesian RL Models

The Stan models implement hierarchical Bayesian frameworks that capture:

- **Player-specific learning rates** varying by controllability context
- **Prediction error responses** modulated by situational control
- **Serve performance dynamics** following wins/losses
- **Individual differences** in emotional regulation and learning

### Model Variants
- `surprise_serve_lr.stan`: Core controllability-dependent learning model
- `surprise_serve_lr_progress_*.stan`: Extensions incorporating match progression
- `control_uniform.stan`: Control model with uniform learning rates

## 📊 Key Findings

**Controllability-Dependent Learning:**
- High control situations: Performance improvements following negative prediction errors
- Low control situations: Performance improvements following positive prediction errors
- Player-specific variation in controllability sensitivity

**Behavioral Evidence:**
- Serve speed adjustments align with theoretical predictions
- Match context significantly moderates learning responses
- Individual differences in emotional regulation patterns

## 📧 Contact

For questions about the Bayesian models, Stan implementation, or research collaboration: [GitHub Issues](https://github.com/lsolomyak/tennis_RL_Bayes/issues)

---
**Keywords**: Bayesian Statistics, Reinforcement Learning, Stan, Sports Psychology, Hierarchical Models, Tennis Analytics
# Power outline

- Sanitize input: Map single letters to test names, convert size-one groups to Bonferroni, put success function in a list
- Validate input
- Simulate p-values (Convert marginal power to non-centrality parameter)
- If all Bonferroni...
  - Run power with shortcut testing
- Otherwise...
  - Bonferroni critical values - just the weights of the closure for the Bonferroni hypotheses
  - Parametric critical values - calculated once with `calculate_critical_parametric()`
    - This uses the c-value calculation method _on each parametric group_
  - _Simes critical values are not calculated here_
    - Weights and simulated p-values for the Simes hypotheses are separated out
    - Simes hypothesis numbers get a custom re-assignment which is their relative position within all Simes hypotheses
  - Loop over simulations
    - Simes critical values - calculated for each simulation with `calculate_critical_simes()`
      - Output includes critical values for missing hypotheses - these get removed
    - Combine all critical values back together, replace NA or incorrect values with 0, and test
- Summarize results, including applying success functions
  

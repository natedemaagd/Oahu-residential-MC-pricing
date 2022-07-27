
bill_calculator_scheduleR <- function(customer_charge_dollars, demand_response_adjustment_clause_cents,
                                      dsm_adjustment_cents, ecrf_cents, green_infrastructure_fee_dollars,
                                      nonfuel_fuel_energy_block1_charge_cents, nonfuel_fuel_energy_block2_charge_cents, nonfuel_fuel_energy_block3_charge_cents,
                                      nonfuel_fuel_energy_block1_qtyKwh, nonfuel_fuel_energy_block2_qtyKwh,
                                      pbf_surcharge_cents, purchase_power_adjustment_cents, rba_rate_adjustment_cents,
                                      kwh){
  
  # bill will be broken up into fixed and variable charges
  
  
  
  
  ##### basic bill calculation #####
  
  # fixed: customer charge, green infrastructure_fee
  FixedCharge_dollars <- customer_charge_dollars + green_infrastructure_fee_dollars
  
  # all other miscellaneous charges are based on customer's kWh usage
  VariableChargeMiscellaneous_cents <- kwh*(demand_response_adjustment_clause_cents +
                                            dsm_adjustment_cents + ecrf_cents +
                                            pbf_surcharge_cents +
                                            purchase_power_adjustment_cents +
                                            rba_rate_adjustment_cents)
  
  # add actual consumption (fuel and non-fuel charges) according to block structure
  # if consumption in first block
  if(kwh <= nonfuel_fuel_energy_block1_qtyKwh){
    
    VariableChargeFuelNonFuel_cents <- kwh * nonfuel_fuel_energy_block1_charge_cents
  
  # if consumption in second block    
  } else if(kwh > nonfuel_fuel_energy_block1_qtyKwh & kwh <= nonfuel_fuel_energy_block2_qtyKwh){
    
                         # (charge from all of block 1) + (charge for portion of consumption in block 2)
    VariableChargeFuelNonFuel_cents <-
      (nonfuel_fuel_energy_block1_qtyKwh * nonfuel_fuel_energy_block1_charge_cents) +
      ((kwh - nonfuel_fuel_energy_block1_qtyKwh) * nonfuel_fuel_energy_block2_charge_cents)
  
  # if consumption in third block      
  } else if(kwh > nonfuel_fuel_energy_block2_qtyKwh){
    
    # (charge from all of block 1) + (charge from all of block 2) + (charge for portion of consumption in block 3)
    VariableChargeFuelNonFuel_cents <-
      (nonfuel_fuel_energy_block1_qtyKwh * nonfuel_fuel_energy_block1_charge_cents) +
      ((nonfuel_fuel_energy_block2_qtyKwh - nonfuel_fuel_energy_block1_qtyKwh) * nonfuel_fuel_energy_block2_charge_cents) +
      ((kwh - nonfuel_fuel_energy_block2_qtyKwh) * nonfuel_fuel_energy_block3_charge_cents)
  
  }
  
  
  
  
  ##### finalize output #####
  
  # add charges to get total charge in dollars
  VariableCharge_cents <- VariableChargeMiscellaneous_cents + VariableChargeFuelNonFuel_cents  # get total variable component (miscellaneous + fuel and non-fuel charges)
  
  # calculate total bill
  subtotal <- FixedCharge_dollars + (VariableCharge_cents/100)  # total of fixed and variable components converted to dollars
  final_bill <- ifelse(subtotal < 25, 25, subtotal)  # minimum bill is $25/mo, so return 25 if subtotal is less than that
  
  # output: final bill
  return(final_bill)
}

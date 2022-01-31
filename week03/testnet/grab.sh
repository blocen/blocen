cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 02.addr) \
    --tx-in b68aaa8f70580c6fcc6c7a87e485e798873d76767099bdc234e76e4d38404fa1#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --tx-in-collateral 76656564392f4836717c134267be9cfadde0ba6908855f4af9066801f8078269#1 \
    --required-signer-hash 8a37c3da37a62763fd6d1c097c154ccd1a623c88fe698927daae7f8a \
    --invalid-before 48866954 \
    --protocol-params-file protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed

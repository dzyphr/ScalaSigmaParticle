import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import scorex.crypto.hash.{Blake2b256}
object pinLock {

  private def pinLock(configFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"

    val lockTime: Int = config.getParameters.get("lockTime").toInt
    val sender: Address = Address.create(config.getParameters.get("senderAddr"))
    val addrIndex: Int = config.getParameters.get("addrIndex").toInt
    

    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)


      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmount: Long = Parameters.OneErg * 2
      val ergoAmountFeeIncluded: Long = ergoAmount + Parameters.MinFee
      val pinLockScript: String =
        s"""
            { sigmaProp(INPUTS(0).R4[Coll[Byte]].get == blake2b256(OUTPUTS(0).R4[Coll[Byte]].get)) }
        """.stripMargin
      val pin = config.getParameters.get("pin").toString
      val hashedPin = Blake2b256(pin.getBytes())
      val contract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.create()
          .build(),
        pinLockScript)

      val inputboxes = BoxOperations.createForSender(sender, ctx)
        .withAmountToSpend(ergoAmountFeeIncluded)
        .loadTop()

      val pinLockBox = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outBoxBuilder()
        .value(ergoAmountFeeIncluded)
        .contract(contract)
        .registers(ErgoValue.of(Blake2b256.hash(pin))).build()

      val unsignedTransaction = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .fee(Parameters.MinFee)
        .outputs(pinLockBox)
        .sendChangeTo(sender.getErgoAddress)
        .build()

      val signedContractTx: SignedTransaction = prover.sign(unsignedTransaction)

      ctx.sendTransaction(signedContractTx)

      signedContractTx.toJson(true)

    })
    txJson
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      println("too many args, enter one arg for config file name")
      sys.exit
    }
    else if (args.length == 0) {
      print("enter the config file name as the only argument")
      sys.exit
    }
    val txJson: String = pinLock(args(0))
    println(txJson)
  }
}
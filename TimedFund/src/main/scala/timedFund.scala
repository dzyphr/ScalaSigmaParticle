import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract

object timedFund {
  private def timedFundDeposit(ConfigFileName: String): String = {
      val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
      val nodeConfig: ErgoNodeConfig = config.getNode
      val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"
      val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

      val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
        val sender: Address = Address.create(config.getParameters.get("senderAddr"))
        val addrIndex: Int = config.getParameters.get("addrIndex").toInt
        val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
        val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
        val ssM: SecretString = SecretString.create(walletMnemonic)
        val ssP: SecretString = SecretString.create(mnemonicPassword)


        val prover: ErgoProver = ctx.newProverBuilder()
          .withMnemonic(ssM, ssP, false)
          .withEip3Secret(addrIndex)
          .build()
        val senderEIP3: Address = prover.getEip3Addresses().get(addrIndex)
        val ergoAmount: Long = Parameters.OneErg * 2
        val ergoAmountFeeIncluded: Long = ergoAmount + Parameters.MinFee
        val timedFundScript: String =
          s"""
              { p1 && sigmaProp(HEIGHT > lockHeight) || p2 && sigmaProp(HEIGHT <= lockHeight)}
          """.stripMargin
        val lockHeight = ctx.getHeight + 5
        val contract: ErgoContract = ctx.compileContract(
          ConstantsBuilder.create()
            .item("p1", sender.getPublicKey)
            .item("p2", Address.create(config.getParameters.get("p2Addr")).getPublicKey)
            .item("lockHeight", lockHeight)
            .build(),
          timedFundScript)

        val inputboxes = BoxOperations.createForSender(sender, ctx)
          .withAmountToSpend(ergoAmountFeeIncluded)
          .loadTop()

        val timedFundBox = ctx.newTxBuilder()
          .boxesToSpend(inputboxes)
          .outBoxBuilder()
          .value(ergoAmountFeeIncluded)
          .contract(contract)
          .build()

        val unsignedTransaction = ctx.newTxBuilder()
          .boxesToSpend(inputboxes)
          .fee(Parameters.MinFee)
          .outputs(timedFundBox)
          .sendChangeTo(senderEIP3.getErgoAddress)
          .build()

        val signedContractTx: SignedTransaction = prover.sign(unsignedTransaction)

        ctx.sendTransaction(signedContractTx)

        signedContractTx.toJson(true)
        //TODO ECHO or FIND & REPLACE TIMEDFUNDBOXID INTO CONFIG AFTER SUCCESS
      })
      txJson
  }

  def timedFundP2Withdraw(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val sender: Address = Address.create(config.getParameters.get("p2Addr"))
      val addrIndex: Int = config.getParameters.get("p2AddrIndex").toInt
      val walletMnemonic: String = config.getParameters.get("p2Mnemonic").toString
      val mnemonicPassword: String = config.getParameters.get("p2MnemonicPassword").toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val timedFundBoxID: String = config.getParameters.get("timedFundBoxID").toString
      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      val ergoAmountFeeSubtracted: Long = Parameters.OneErg  - Parameters.MinFee
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted)
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .build()
      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(timedFundBoxID).array.last)
      val tx = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outputs(unlockBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      val signed = prover.sign(tx)

      ctx.sendTransaction(signed)

      signed.toJson(true)
    })
    txJson
  }

  def timedFundP1ReClaim(ConfigFileName: String): String = {
    val config: ErgoToolConfig = ErgoToolConfig.load(ConfigFileName)
    val nodeConfig: ErgoNodeConfig = config.getNode
    val explorerUrl: String = "https://tn-ergo-explorer.anetabtc.io/"

    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConfig, explorerUrl)

    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {
      val sender: Address = Address.create(config.getParameters.get("senderAddr"))
      val addrIndex: Int = config.getParameters.get("addrIndex").toInt
      val walletMnemonic: String = nodeConfig.getWallet().getMnemonic().toString
      val mnemonicPassword: String = nodeConfig.getWallet().getMnemonicPassword().toString
      val ssM: SecretString = SecretString.create(walletMnemonic)
      val ssP: SecretString = SecretString.create(mnemonicPassword)
      val timedFundBoxID: String = config.getParameters.get("timedFundBoxID").toString

      val prover: ErgoProver = ctx.newProverBuilder()
        .withMnemonic(ssM, ssP, false)
        .withEip3Secret(addrIndex)
        .build()
      //val ergoAmountFeeSubtracted: Long = Parameters.OneErg - Parameters.MinFee//MAX
      val ergoAmountFeeSubtracted: Long = Parameters.OneErg - Parameters.MinFee//HALF
      //use same contract conditions
      val unlockBox = ctx.newTxBuilder().outBoxBuilder()
        .value(ergoAmountFeeSubtracted - Parameters.MinFee)//TODO STOP DUST CONTRACT ACCUMULATION
        .contract(new ErgoTreeContract(sender.getErgoAddress.script, NetworkType.TESTNET))
        .build()
      val inputboxes = java.util.Arrays.asList(ctx.getBoxesById(timedFundBoxID).array.last)
      val tx = ctx.newTxBuilder()
        .boxesToSpend(inputboxes)
        .outputs(unlockBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      val signed = prover.sign(tx)

      ctx.sendTransaction(signed)

      signed.toJson(true)
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
    //currently you can switch between noted function calls to switch between parties
    val txJson: String = timedFundDeposit(args(0))
    //val txJson: String = timedFundP2Withdraw(args(0))
    //val txJson: String = timedFundP1ReClaim(args(0))
    println(txJson)
  }
}


Sub-commands and Arguments
==========================
Common arguments to all sub-commands:


* :code:`-q` or :code:`--quiet`   Set verbosity level to 0

* :code:`--switch STRING`   Set switch

* :code:`-v` or :code:`--verbose`   Increase verbosity level

Overview of sub-commands::
  
  account copy from
    Copy accounts from another switch to the current one
  
  account create
    Create new accounts in the wallet
  
  account info
    Get account info (local or from blockchain).
  
  account list
    List all known accounts in the current switch
  
  account remove
    Delete accounts from the wallet
  
  account rename
    This command renames accounts in the wallet
  
  account set
    Modify the information associated with accounts in the wallet
  
  account whois
    Find accounts matching a string
  
  call
    Call contracts
  
  client
    Call tonos-cli, use -- to separate arguments. Use 'ft exec -- CMD ARGS' for other commands.
  
  config
    Modify configuration
  
  contract abi
    Print contract ABI
  
  contract abi impl
    Generate an implementation source file from a contract ABI
  
  contract abi intf
    Generate an interface source file from a contract ABI
  
  contract build
    Build a contract
  
  contract deploy
    Deploy contracts
  
  contract import
    Import a contract
  
  contract list
    List known contracts
  
  contract new
    Generate a new contract source file
  
  contract new interface
    Generate a new contract interface source file
  
  crawler
    Crawl all transactions to an address and fill a psql database
  
  debot
    Manage debots
  
  exec
    Call command with substitution on arguments, use -- before the command.
  
  init
    Initialize with TON Labs binary tools, compiled from sources.
  
  inspect
    Inspect information stored on the blockchain: display information on accounts, blocks, messages and transactions.
  
  multisig confirm
    Confirm transactions on a multisig-wallet
  
  multisig create
    Manage a multisig-wallet (create, confirm, send)
  
  multisig debot
    Executes the multisig debot
  
  multisig list custodians
    List owners/custodians of a multisig wallet
  
  multisig list transactions
    Display waiting transactions in a multisig wallet
  
  multisig transfer
    Transfer TONs from a multisig wallet to another account
  
  node give
    Give TONs to accounts on sandbox networks
  
  node live
    Open Node Live block explorer webpage
  
  node start
    Start the node of a local sandbox switch
  
  node stop
    Stop the local node of a sandbox switch
  
  node update
    Update Docker image of TONOS SE for new features. You must recreate sandbox switches to benefit from the new image.
  
  node web
    Open Node GraphQL webpage
  
  output
    Perform substitutions on the output
  
  print error
    Display error codes
  
  switch create
    Create a new switch for an existing network, or create a sandbox local network
  
  switch list
    List the current switches/networks
  
  switch remove
    Remove a network configurations/switches
  
  switch to
    Switch to another network/switch
  
  test
    For testing only
  
  utils
    Some useful tools
  
  watch
    Monitor a given account for new transactions.


ft account copy from
~~~~~~~~~~~~~~~~~~~~~~

Copy accounts from another switch to the current one



**DESCRIPTION**


This command copies accounts from another switch to the current one

Examples:
::

  ft account copy mainnet my-account

::

  ft --switch testnet account copy my-account


**USAGE**
::
  
  ft account copy from SWITCH ACCOUNTS [OPTIONS]

Where options are:


* :code:`SWITCH ACCOUNTS`   Switch name and accounts

* :code:`--prefix PREFIX`   Prefix created accounts by PREFIX

* :code:`--rename ACCOUNT`   New name of account


ft account create
~~~~~~~~~~~~~~~~~~~

Create new accounts in the wallet



**DESCRIPTION**


This command creates new accounts in the wallet

Examples:
::

  ft account create account1 account2 account3

::

  ft account create new-account --passphrase "some known passphrase" --contract SetcodeMultisigWallet2

::

  ft account create new-address --address 0:1234... --surf


The accounts are created in the wallet, not in the blockchain. To create accounts on the blockchain, you need to transfer funds to the account address and deploy a contract (for example, with 'ft multisig create')

**USAGE**
::
  
  ft account create ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Name of account

* :code:`--address ADDRESS`   Address for account

* :code:`--contract CONTRACT`   Contract for account

* :code:`--force` or :code:`-f`   Override existing contracts with --create

* :code:`--keyfile KEYFILE`   Key file for account

* :code:`--multisig`   Contract should be multisig

* :code:`--passphrase PASSPHRASE`   BIP39 Passphrase for account

* :code:`--static-vars JSON`   Set static vars for account

* :code:`--surf`   Contract should be TON Surf contract

* :code:`--wc WORKCHAIN`   The workchain (default is 0)


ft account info
~~~~~~~~~~~~~~~~~

Get account info (local or from blockchain).



**DESCRIPTION**


This command displays information on given accounts, either locally or from the blockchain

Examples:
::

  ft account info MY-ACCOUNT

::

  ft account info MY-ACCOUNT --all


**USAGE**
::
  
  ft account info ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Name of account

* :code:`--all`   Display all account parameters

* :code:`--live`   Open block explorer on address


ft account list
~~~~~~~~~~~~~~~~~

List all known accounts in the current switch



**DESCRIPTION**


This command lists all known accounts in the wallet.

Examples:
::

  ft account list


pass: passphrase known

secr: secret key known

pk: public key

ad: address known

**USAGE**
::
  
  ft account list [OPTIONS]

Where options are:



ft account remove
~~~~~~~~~~~~~~~~~~~

Delete accounts from the wallet



**DESCRIPTION**


This command deletes known accounts from the wallet.

Examples:
::

  ft account remove account1 account2


**USAGE**
::
  
  ft account remove ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Name of account


ft account rename
~~~~~~~~~~~~~~~~~~~

This command renames accounts in the wallet



**DESCRIPTION**


This command renames accounts in the wallet

Examples:
::

  ft account rename old-name new-name

::

  ft account rename test1 test2 test3 --prefix old-


**USAGE**
::
  
  ft account rename ACCOUNTS [OPTIONS]

Where options are:


* :code:`ACCOUNTS`   Source and Destination accounts

* :code:`--prefix PREFIX`   Prefix provided accounts by PREFIX


ft account set
~~~~~~~~~~~~~~~~

Modify the information associated with accounts in the wallet



**DESCRIPTION**


This command adds information to existing accounts in the wallet

Examples:
::

  ft account set old-account --contract SafeMultisigWallet


**USAGE**
::
  
  ft account set ARGUMENT [OPTIONS]

Where options are:


* :code:`ARGUMENT`   Name of account

* :code:`--address ADDRESS`   Address for account

* :code:`--contract CONTRACT`   Contract for account

* :code:`--keyfile KEYFILE`   Key file for account

* :code:`--multisig`   Contract should be multisig

* :code:`--passphrase PASSPHRASE`   BIP39 Passphrase for account

* :code:`--static-vars JSON`   Set static vars for account

* :code:`--surf`   Contract should be TON Surf contract

* :code:`--wc WORKCHAIN`   The workchain (default is 0)


ft account whois
~~~~~~~~~~~~~~~~~~

Find accounts matching a string



**DESCRIPTION**


This command searches existing accounts for a field matching the string

Examples:
::

  ft account whois 1234

::

  ft account whois 0:1234

::

  ft account whois setcode


**USAGE**
::
  
  ft account whois ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Name of account


ft call
~~~~~~~~~

Call contracts



**DESCRIPTION**


Call a method of a deployed contract. Use --local or --run to run the contract locally (only for get methods). If the params are not specified, {} is used instead. The message is signed if the --sign SIGNER argument is provided, or if the secret key of the account is known.

Examples:
::

  $ ft call giver sendGrams
          '{ "dest":"%{account:address:user1}", "amount":"1000000000000"}'

::

  $ ft --switch mainnet call msig confirmUpdate
          '{  "updateId": "0x6092b3ee656aaa81" }' --sign mywallet


**USAGE**
::
  
  ft call ACCOUNT METH [JSON_PARAMS] [OPTIONS]

Where options are:


* :code:`ACCOUNT METH [JSON_PARAMS]`   arguments

* :code:`-o FILE` or :code:`--output FILE`   Save result to FILE (use - for stdout)

* :code:`--run` or :code:`--local`   Run locally

* :code:`--sign ACCOUNT`   Sign message with account

* :code:`--subst FILE`   Read FILE and substitute results in the content

* :code:`--wait`   Wait for all transactions to finish


ft client
~~~~~~~~~~~

Call tonos-cli, use -- to separate arguments. Use 'ft exec -- CMD ARGS' for other commands.



**DESCRIPTION**


This command calls the tonos-cli executable while performing substitutions on arguments, and using the node of the current network switch. It is useful for commands that 'ft' cannot perform directly (calling debots for example).

'ft' uses the executable stored in $HOME/.ft/bin/tonos-cli. To create this executable, use:
::

  $ ft init


or:
::

  $ ft init --client


The available substitutions on the arguments can be listed using:
::

  $ ft output --list-subst


For example, to substitute the address of the account 'multisig-debot':
::

  $ ft client -- debot fetch %{account:address:multisig-debot}


Note that it is also possible to ask 'ft' to call 'tonos-cli' instead of performing calls through TON-SDK Rust binding for other commands, using the FT_USE_TONOS=1 env. variable.

**USAGE**
::
  
  ft client -- ARGUMENTS [OPTIONS]

Where options are:


* :code:`-- ARGUMENTS`   Arguments to tonos-cli

* :code:`--exec`   (deprecated, use 'ft exec -- COMMAND' instead)

* :code:`--stdout FILE`   Save command stdout to file


ft config
~~~~~~~~~~~

Modify configuration



**DESCRIPTION**


Change the global configuration or the network configuration.

**USAGE**
::
  
  ft config [OPTIONS]

Where options are:


* :code:`--deployer ACCOUNT`   Set deployer to account ACCOUNT. The deployer is the account used to credit the initial balance of an address before deploying a contract on it.


ft contract abi
~~~~~~~~~~~~~~~~~

Print contract ABI



**DESCRIPTION**


This command shows a human readable version of contract ABI

**USAGE**
::
  
  ft contract abi CONTRACT [OPTIONS]

Where options are:


* :code:`CONTRACT`   Name of contract to build


ft contract abi impl
~~~~~~~~~~~~~~~~~~~~~~

Generate an implementation source file from a contract ABI



**DESCRIPTION**


This command generates an implementation source file xfrom a contract ABI

**USAGE**
::
  
  ft contract abi impl CONTRACT [OPTIONS]

Where options are:


* :code:`CONTRACT`   Name of contract to build


ft contract abi intf
~~~~~~~~~~~~~~~~~~~~~~

Generate an interface source file from a contract ABI



**DESCRIPTION**


This command generates an interface source file from a contract ABI

**USAGE**
::
  
  ft contract abi intf CONTRACT [OPTIONS]

Where options are:


* :code:`CONTRACT`   Name of contract to build


ft contract build
~~~~~~~~~~~~~~~~~~~

Build a contract



**DESCRIPTION**


This command builds a Solidity contract and store it in the contract database

Example:
::

  ft contract build Foobar.sol


After this command, the contract will be known as 'Foobar' in the contract database

**USAGE**
::
  
  ft contract build FILENAME [OPTIONS]

Where options are:


* :code:`FILENAME`   Build this contract and remember it

* :code:`--contract CONTRACT`   Name of contract to build

* :code:`--force` or :code:`-f`   Override existing contracts


ft contract deploy
~~~~~~~~~~~~~~~~~~~~

Deploy contracts



**DESCRIPTION**


This command deploys a known contract to the blockchain

Examples:
::

  ft contract deploy Forbar


Create an account 'Foorbar', deploy a contract 'Foobar' to it.
::

  ft contract deploy Forbar --create foo


Create an account 'foo', deploy a contract 'Foobar' to it.
::

  ft contract deploy Forbar --replace foo


Delete account 'foo', recreate it and deploy a contract 'Foobar' to it.
::

  ft contract deploy Forbar --create foo --sign admin


Create an empty account 'foo', deploy a contract 'Foobar' to it, using the keypair from 'admin'.
::

  ft contract deploy Forbar --dst foo


Deploy a contract 'Foobar' an existing account 'foo' using its keypair.



With --create and --replace, 1 TON is transferred to the initial account using a 'deployer' multisig account. The deployer account can either be set switch wide (ft config --deployer 'account') or in the deploy command (using the --deployer 'account' argument)

**USAGE**
::
  
  ft contract deploy CONTRACT PARAMS [OPTIONS]

Where options are:


* :code:`CONTRACT`   Deploy contract CONTRACT

* :code:`PARAMS`   Constructor/call Arguments ({} by default)

* :code:`--create ACCOUNT`   Create ACCOUNT by deploying contract (with --deploy)

* :code:`--credit TONS`   Initial credit of the account by the deployer

* :code:`--deployer ACCOUNT`   Deployer is this account (pays creation fees)

* :code:`--dst ACCOUNT`   Deploy to this account, using the existing keypair

* :code:`--force` or :code:`-f`   Override existing contracts

* :code:`--params PARAMS`   Constructor/call Arguments ({} by default)

* :code:`--replace ACCOUNT`   Replace ACCOUNT when deploying contract (with --deploy)

* :code:`--sign ACCOUNT`   Deploy using this keypair

* :code:`--static-vars JSON`   Set static vars for account


ft contract import
~~~~~~~~~~~~~~~~~~~~

Import a contract



**DESCRIPTION**


This command imports a contract into the contract database

Example:
::

  ft contract import src/Foo.tvm


Import the given contract into the contract database. Two files are mandatory: the ABI file and the TVM file. They should be stored in the same directory. The ABI file must use either a '.abi' or '.abi.json' extension, whereas the TVM file must use either '.tvc' or '.tvm. If a source file (.sol, .cpp, .hpp) is also present, it is copied in the database.

**USAGE**
::
  
  ft contract import FILENAME [OPTIONS]

Where options are:


* :code:`FILENAME`   Import contract from FILENAME


ft contract list
~~~~~~~~~~~~~~~~~~

List known contracts


**USAGE**
::
  
  ft contract list [OPTIONS]

Where options are:



ft contract new
~~~~~~~~~~~~~~~~~

Generate a new contract source file


**USAGE**
::
  
  ft contract new CONTRACT [OPTIONS]

Where options are:


* :code:`CONTRACT`   Create contract file for CONTRACT


ft contract new interface
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generate a new contract interface source file


**USAGE**
::
  
  ft contract new interface CONTRACT [OPTIONS]

Where options are:


* :code:`CONTRACT`   Create interface file for contract


ft crawler
~~~~~~~~~~~~

Crawl all transactions to an address and fill a psql database



**DESCRIPTION**


This command will crawl the blockchain and fill a PostgresQL database with all events related to the contract given in argument. The created database has the same name as the account.

This command can run as a service, using the --start command to launch a manager program (that will not detach itself, however), --status to check the current status (running or not) and --stop to stop the process and its manager.

A simple session looks like:
::

  sh> ft crawler myapp --start &> daemon.log &
  sh> psql myapp
  SELECT * FROM freeton_events;
  serial|                              msg_id                              |      event_name       |           event_args                            |    time    | tr_lt
      1 | ec026489c0eb2071b606db0c7e05e5a76c91f4b02c2b66af851d56d5051be8bd | OrderStateChanged     | {"order_id":"31","state_count":"1","state":"1"} | 1620744626 | 96
  SELECT * FROM freeton_transactions;
  ^D
  sh> ft crawler myapp --stop
  



**ERRORS**


The crawler may fail connecting to the database. You can use PGHOST to set the hostname of the database, or the directory of unix sockets (default is /var/run/postgresql). You can use PGPORT for the port (default is 5432).

The crawler may also fail for authorizations (something like FATAL: 28000: role USER does not exist ). In such a case, you need to configure postgresql to allow your role (<user> is your username):
::

  
       sh> sudo -i -u postgres
       root> psql
       CREATE USER <user>;
       ALTER ROLE <user> CREATEDB;
  


**USAGE**
::
  
  ft crawler ACCOUNT [OPTIONS]

Where options are:


* :code:`ACCOUNT`   Account to crawl

* :code:`--dropdb`   Drop the previous database

* :code:`--start`   Start with a manager process to restart automatically

* :code:`--status`   Check if a manager process and crawler are running

* :code:`--stop`   Stop the manager process and the crawler


ft debot
~~~~~~~~~~

Manage debots



**DESCRIPTION**


**USAGE**
::
  
  ft debot [OPTIONS]

Where options are:


* :code:`--new NAME`   Create template files for debot NAME


ft exec
~~~~~~~~~

Call command with substitution on arguments, use -- before the command.



**DESCRIPTION**


This command can be used to call external commands while performing substitutions on arguments.

The available substitutions on the arguments can be listed using:
::

  $ ft output --list-subst


For example:

$ ft exec -- echo %{account:address:giver}

**USAGE**
::
  
  ft exec -- COMMAND ARGUMENTS [OPTIONS]

Where options are:


* :code:`-- COMMAND ARGUMENTS`   Command and arguments

* :code:`--stdout FILENAME`   Save command stdout to file FILENAME


ft init
~~~~~~~~~

Initialize with TON Labs binary tools, compiled from sources.



**DESCRIPTION**


Initialize with TON Labs binary tools, downloading them from their GIT repositories and compiling them (a recent Rust compiler must be installed).

Tools are installed in $HOME/.ft/bin/.

The following tools can be installed:

* 1.
  The 'tonos-cli' client

* 2.
  The 'solc' client from the TON-Solidity-Compiler repository

* 3.
  The 'tvm_linker' encoder from the TVM-linker repository

If no specific option is specified, all tools are generated. If a tool has already been generated, calling it again will try to upgrade to a more recent version.

**USAGE**
::
  
  ft init [OPTIONS]

Where options are:


* :code:`--client`   Build and install 'tonos-cli' from sources

* :code:`--code-hashes`   Create a database of code hashes from predefined contracts

* :code:`--distclean`   Clean completely before building

* :code:`--linker`   Build and install 'tvm_linker' from sources

* :code:`--solc`   Build and install 'solc' from sources


ft inspect
~~~~~~~~~~~~

Inspect information stored on the blockchain: display information on accounts, blocks, messages and transactions.



**DESCRIPTION**


Inspect information stored on the blockchain: display information on accounts, blocks, messages and transactions.

Examples:

Display all transactions that happened on the user1 account:
::

  $ ft inspect --past user1 --with deployed:Contract


The --with argument is used to name the first unknown address, with the name 'deployed' and type 'Contract'. Messages sent to known accounts with known contract types are automatically decoded.

Some operations (--block-num and --head) require to know the shard on which they apply. Arguments --shard SHARD, --shard-block BLOCK_ID and --shard-account ACCOUNT can be used to specify the shard.

Use the FT_DEBUG_GRAPHQL=1 variable to show Graphql queries

**USAGE**
::
  
  ft inspect [OPTIONS]

Where options are:


* :code:`-2`   Verbosity level 2

* :code:`-3`   Verbosity level 3

* :code:`-4`   Verbosity level 4

* :code:`-a ACCOUNT` or :code:`--account ACCOUNT`   Inspect state of account ACCOUNT (or 'all') on blockchain

* :code:`--abis ABI`   Shared ABIs. Useful for example if you expect to receive messages that your contract does not implement (IParticipant for SafeMultisigWallet, for example)

* :code:`-b BLOCK` or :code:`--block BLOCK`   BLOCK Inspect block TR_ID on blockchain

* :code:`--bn BLOCK_NUM` or :code:`--block-num BLOCK_NUM`   Inspect block at level BLOCK_NUM on blockchain

* :code:`-h` or :code:`--head`   Inspect head

* :code:`--limit NUM`   Limit the number of results to NUM

* :code:`-m MSG_ID` or :code:`--message MSG_ID`   Inspect message with identifier MSG_ID on blockchain

* :code:`-o FILE` or :code:`--output FILE`   Save result to FILE (use - for stdout)

* :code:`--past ACCOUNT`   Inspect past transactions on ACCOUNT on blockchain

* :code:`--shard SHARD`   Block info level/head for this shard

* :code:`--shard-account ACCOUNT`   Block info level/head for this shard

* :code:`--shard-block BLOCK_ID`   Block info level/head for this shard

* :code:`--subst FILE`   Read FILE and substitute results in the content

* :code:`-t TR_ID` or :code:`--transaction TR_ID`   Inspect transaction with identifier TR_ID on blockchain

* :code:`--with ACCOUNT:CONTRACT`   Define partner account automatically defined


ft multisig confirm
~~~~~~~~~~~~~~~~~~~~~

Confirm transactions on a multisig-wallet



**DESCRIPTION**


This command is used to confirm transactions on a multisig wallet.


**LIST WAITING TRANSACTIONS**


Display transactions waiting for confirmations:
::

  # ft multisig list transactions MY-ACCOUNT



**CONFIRM TRANSACTION**


Get the transaction ID from above, and use:
::

  # ft multisig confirm MY-ACCOUNT TX_ID


**USAGE**
::
  
  ft multisig confirm ACCOUNT TX_ID [OPTIONS]

Where options are:


* :code:`ACCOUNT TX_ID`   The multisig account and the TX_ID

* :code:`--src ACCOUNT`   The multisig account


ft multisig create
~~~~~~~~~~~~~~~~~~~~

Manage a multisig-wallet (create, confirm, send)



**DESCRIPTION**


This command deploys a multisig contract on a credited account.

Create an account and get its address:
::

  # ft account create MY-ACCOUNT


Backup the account info off-computer.

The second command will give you an address in 0:XXX format. Send some tokens on the address to be able to deploy the multisig.

Check its balance with:
::

  # ft account info MY-ACCOUNT


Then, to create a single-owner multisig:
::

  # ft multisig create MY-ACCOUNT


To create a multi-owners multisig:
::

  # ft multisig create MY-ACCOUNT owner2 owner3 owner4


To create a multi-owners multisig with 2 signs required:
::

  # ft multisig create MY-ACCOUNT owner2 owner3 --req 2


To create a multi-owners multisig not self-owning:
::

  # ft multisig create MY-ACCOUNT owner1 owner2 owner3 --not-owner


Verify that it worked:
::

  # ft account info MY-ACCOUNT -v


**USAGE**
::
  
  ft multisig create ACCOUNT [OPTIONS]

Where options are:


* :code:`ACCOUNT`   Account name, and other custodians

* :code:`--contract CONTRACT`   Use this contract

* :code:`--not-owner`    Initial account should not be an owner

* :code:`--req REQ`   Number of confirmations required

* :code:`--surf`   Use Surf contract

* :code:`--wc WORKCHAIN`   The workchain (default is 0)


ft multisig debot
~~~~~~~~~~~~~~~~~~~

Executes the multisig debot



**DESCRIPTION**


This command executes the multisig debot, whose address is in the 'debot-multisig' account. 

**USAGE**
::
  
  ft multisig debot ACCOUNT [OPTIONS]

Where options are:


* :code:`ACCOUNT`   The debot account


ft multisig list custodians
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

List owners/custodians of a multisig wallet



**DESCRIPTION**


This command can be used to display the pubkeys of the owners/custodians of a multisig wallet

To get the list of signers:
::

  # ft multisig list custodians MY-ACCOUNT"


**USAGE**
::
  
  ft multisig list custodians ACCOUNT [OPTIONS]

Where options are:


* :code:`ACCOUNT`   The multisig account


ft multisig list transactions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Display waiting transactions in a multisig wallet



**DESCRIPTION**


This command can be used to display the currently waiting transactions in a multisig wallet


**LIST WAITING TRANSACTIONS**


Display transactions waiting for confirmations:
::

  # ft multisig list transactions MY-ACCOUNT


**USAGE**
::
  
  ft multisig list transactions ACCOUNT [OPTIONS]

Where options are:


* :code:`ACCOUNT`   The multisig account


ft multisig transfer
~~~~~~~~~~~~~~~~~~~~~~

Transfer TONs from a multisig wallet to another account



**DESCRIPTION**


This command is used to send tokens from a multisig wallet to another account (or to submit a transaction if multiple confirmations are required).


**SIMPLE TRANSFER**


Should be like that:
::

  # ft multisig transfer 100.000 -from MY-ACCOUNT --to OTHER-ACCOUNT


If the target is not an active account:
::

  # ft multisig transfer 100.000 --from MY-ACCOUNT --to OTHER-ACCOUNT --parrain


To send all the balance:
::

  # ft multisig transfer all --from MY-ACCOUNT --to OTHER-ACCOUNT



**CALL WITH PARAMS**


Should be like that:
::

  # ft multisig transfer 100 --from MY-ACCOUNT --to CONTRACT set '{ "x": "100" }


**USAGE**
::
  
  ft multisig transfer ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   Generic arguments

* :code:`--bounce BOOL`   BOOL Transfer to inactive account

* :code:`--from ACCOUNT`   The source of the transfer

* :code:`--parrain`    Transfer to inactive account

* :code:`--src ACCOUNT`   The custodian signing the multisig transfer

* :code:`--to ACCOUNT`   Target of a transfer

* :code:`--wait`   Wait for all transactions to finish


ft node give
~~~~~~~~~~~~~~

Give TONs to accounts on sandbox networks



**DESCRIPTION**


This command can be used to create initial accounts with tokens on sandbox networks running TONOS SE (created using 'ft switch create sandboxNN').

**USAGE**
::
  
  ft node give ACCOUNT [OPTIONS]

Where options are:


* :code:`ACCOUNT`   Give TONs from giver to ACCOUNT (all if none specified). By default, transfer 1000 TONS to the account if its balance is smaller, and deploy a contract if it is a multisig smart contract.

* :code:`--amount AMOUNT`   Number of TONs to give


ft node live
~~~~~~~~~~~~~~

Open Node Live block explorer webpage



**DESCRIPTION**


Open the block explorer available on the sandbox switch running TONOS SE

**USAGE**
::
  
  ft node live [OPTIONS]

Where options are:



ft node start
~~~~~~~~~~~~~~~

Start the node of a local sandbox switch



**DESCRIPTION**


This command can be used to start the node of a local sandbox network running TONOS SE and created using 'ft switch create sandboxNN'.

**USAGE**
::
  
  ft node start [OPTIONS]

Where options are:



ft node stop
~~~~~~~~~~~~~~

Stop the local node of a sandbox switch



**DESCRIPTION**


This command can be used to stop the local node of a sandbox network running TONOS SE

**USAGE**
::
  
  ft node stop [OPTIONS]

Where options are:



ft node update
~~~~~~~~~~~~~~~~

Update Docker image of TONOS SE for new features. You must recreate sandbox switches to benefit from the new image.



**DESCRIPTION**


This command can be used to update the docker image that will be used to create new sandbox networks (tonlabs/local-node, or the one provide with --image)

**USAGE**
::
  
  ft node update [OPTIONS]

Where options are:


* :code:`--image DOCKER`   Docker image to use for sandboxes


ft node web
~~~~~~~~~~~~~

Open Node GraphQL webpage



**DESCRIPTION**


This command can be used to open the GraphQL webpage associated with the current local network

**USAGE**
::
  
  ft node web [OPTIONS]

Where options are:



ft output
~~~~~~~~~~~

Perform substitutions on the output



**DESCRIPTION**


This command performs substitutions on its input. By default, the output goes to stdout, unless the '-o' option is used.

Examples:

Load a file INPUT, substitute its content, and save to OUTPUT:
::

  $ ft output --file INPUT --o OUTPUT


List available substitutions:
::

  $ ft output --list-subst


Output address of account ACCOUNT:
::

  $ ft output --addr ACCOUNT


or:
::

  $ ft output --string %{account:address:ACCOUNT}


Output keyfile of account ACCOUNT to file KEYFILE:
::

   ft output --keyfile ACCOUNT -o KEYFILE


**USAGE**
::
  
  ft output STRING [OPTIONS]

Where options are:


* :code:`STRING`   Output string after substitution

* :code:`--addr ACCOUNT`   Output address of account

* :code:`--file FILE`   Output content of file after substitution

* :code:`--keyfile ACCOUNT`   Output key file of account

* :code:`--list-subst`   List all substitutions

* :code:`-o FILE`   Save command stdout to file

* :code:`--string STRING`   Output string after substitution


ft print error
~~~~~~~~~~~~~~~~

Display error codes


**USAGE**
::
  
  ft print error ERROR CODe [OPTIONS]

Where options are:


* :code:`ERROR CODe`   Error code to explain


ft switch create
~~~~~~~~~~~~~~~~~~

Create a new switch for an existing network, or create a sandbox local network



**DESCRIPTION**


This command is used to create new switches, either for existing remote networks (mainnet, testnet, etc.) by providing their URL with --url, or to create new local networks running TONOS SE (such switches must be called 'sandboxNN' where NN is a number). Each switch includes its own set of accounts and nodes.

When a new switch is created, it immediately becomes the current switch.


**EXAMPLES**


Display current network and other existing networks:
::

  $ ft switch list


Change current network to an existing network NETWORK:
::

  $ ft switch to NETWORK


Create a new network with name NETWORK and url URL, and switch to that network:
::

  $ ft switch create NETWORK --url URL


Removing a created network:
::

  $ ft switch remove NETWORK



**SANDBOXING**


As a specific feature, ft can create networks based on TONOS SE to run on the local computer. Such networks are automatically created by naming the network 'sandboxN` where N is a number. The corresponding node will run on port 7080+N.

Example of session (create network, start node, give user1 1000 TONs):
::

  $ ft switch create sandbox1

::

  $ ft node start

::

  $ ft node give user1 --amount 1000


When a local network is created, it is initialized with:

* 1.
  An account 'giver' corresponding to the Giver contract holding 5 billion TONS

* 2.
  A set of 10 accounts 'user0' to 'user9'. These accounts always have the same secret keys, so it is possible to define test scripts that will work on different instances of local networks.

The 10 accounts are not deployed, but it is possible to use 'ft node give ACCOUNT' to automatically deploy the account.

**USAGE**
::
  
  ft switch create NETWORK [OPTIONS]

Where options are:


* :code:`NETWORK`   Name of network switch to create

* :code:`--image DOCKER`   Docker image to use for sandboxes

* :code:`--url URL`   URL of the default node in this network


ft switch list
~~~~~~~~~~~~~~~~

List the current switches/networks



**DESCRIPTION**


This command is used to list the known networks (switches) and the current configuration.

**USAGE**
::
  
  ft switch list [OPTIONS]

Where options are:



ft switch remove
~~~~~~~~~~~~~~~~~~

Remove a network configurations/switches



**DESCRIPTION**


Remove network configurations


**EXAMPLES**


Removing a created network:
::

  $ ft switch remove NETWORK


**USAGE**
::
  
  ft switch remove NETWORK [OPTIONS]

Where options are:


* :code:`NETWORK`   Name of network switch to remove

* :code:`-f` or :code:`--force`   Remove network even in case of failure


ft switch to
~~~~~~~~~~~~~~

Switch to another network/switch



**DESCRIPTION**


This command is used to switch the current network configuration to the provided switch. To only switch to a new network for a particular command, it is possible to use the --switch argument instead.


**EXAMPLE**


Display current network and other existing networks:
::

  $ ft switch list


Change current network to an existing network NETWORK:
::

  $ ft switch to NETWORK


Call a command for another switch:
::

  $ ft --switch NETWORK call contract get '{}'


**USAGE**
::
  
  ft switch to NETWORK [OPTIONS]

Where options are:


* :code:`NETWORK`   Name of network switch


ft test
~~~~~~~~~

For testing only


**USAGE**
::
  
  ft test ARGUMENTS [OPTIONS]

Where options are:


* :code:`ARGUMENTS`   args

* :code:`--test INT`   NUM Run test NUM


ft utils
~~~~~~~~~~

Some useful tools



**DESCRIPTION**


Misc commands. For example, to translate bytes from base64 or message boc.

**USAGE**
::
  
  ft utils [OPTIONS]

Where options are:


* :code:`--of-base64 STRING`   Translates from base64

* :code:`--of-boc STRING`   Parse message boc in base64 format


ft watch
~~~~~~~~~~

Monitor a given account for new transactions.



**DESCRIPTION**


Wait for transactions happening on the given ACCOUNT. Transactions are immediately displayed on stdout. If the argument --on-event CMD is provided, a command is called for every event emitted by the contract.

**USAGE**
::
  
  ft watch ACCOUNT [OPTIONS]

Where options are:


* :code:`ACCOUNT`   Watch account ACCOUNT

* :code:`-0`   Verbosity level none

* :code:`-3`   Verbosity level 3

* :code:`--account ACCOUNT`   Watch account ACCOUNT

* :code:`--from BLOCKID`   Start with block identifier BLOCKID

* :code:`-o FILE` or :code:`--output FILE`   Output to FILE

* :code:`--on-event CMD`   Call CMD on event emitted. Called once on startup as `CMD <block_id> start` and after every emitted event as `CMD <block_id> <tr_id> <event_name> <args>`

* :code:`--timeout TIMEOUT`   Timeout in seconds (default is 25 days)

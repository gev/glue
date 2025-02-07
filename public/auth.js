const register = async () => {
    try {
        const registerOptions = makeRegisterOptions()
        console.log(registerOptions)
        const publicKeyCredentialCreationOptions = await startRegister(registerOptions)
        console.log(publicKeyCredentialCreationOptions)
        const credentials = await createCredentials(publicKeyCredentialCreationOptions)
        console.log(credentials)
        const attestationResponse = makeAuthenticatorAttestationResponse(credentials)
        console.log(attestationResponse)
        const publicKeyCredentials = makePublicKeyCredentials(credentials, attestationResponse)
        console.log(publicKeyCredentials)
        const res = await finishRegister(publicKeyCredentials)
        console.log(res)
    } catch (err) {
        console.error(err.message)
        debug(err.message)
    }
}

const makeRegisterOptions = () => ({
    login: document.getElementById("login").value,
    name: document.getElementById("name").value,
})

const startRegister = startRegisterOptions =>
    request("/register/start", startRegisterOptions)

const createCredentials = options =>
    navigator.credentials.create({
        publicKey: makePublicKeyCredentialCreationOptions(options)
    })

const makePublicKeyCredentialCreationOptions = options => ({
    ...options,
    user: {
        ...options.user,
        id: fromBase64(options.user.id),
    },
    challenge: fromBase64(options.challenge),
    pubKeyCredParams: [
        {
            type: "public-key",
            alg: -8,
        },
        {
            type: "public-key",
            alg: -7,
        },
        {
            type: "public-key",
            alg: -257,
        }
    ]
})

const makeAuthenticatorAttestationResponse = credentials => ({
    challenge: fromBase64URL(getChallenge(credentials.response.clientDataJSON)),
    publicKey: toBase64(credentials.response.getPublicKey()),
    publicKeyAlgorithm: credentials.response.getPublicKeyAlgorithm(),
})

const finishRegister = finishRequestOptions =>
    request("/register/finish", finishRequestOptions)


const authenticate = async () => {
    try {
        const authenticationOptions = makeAuthenticateOptions()
        console.log(authenticationOptions)
        const publicKeyCredentialRequestOptions = await startAuthenticate(authenticationOptions)
        console.log(publicKeyCredentialRequestOptions)
        const credentials = await getCredentials(publicKeyCredentialRequestOptions)
        console.log(credentials)
        const assertionResponse = await makeAuthenticatorAssertionResponse(credentials)
        console.log(assertionResponse)
        const publicKeyCredentials = await makePublicKeyCredentials(credentials, assertionResponse)
        console.log(publicKeyCredentials)
        const res = await finishAuthenticate(publicKeyCredentials)
        console.log(res)
    } catch (err) {
        console.error(err.message)
        debug(err.message)
    }
}

const makeAuthenticateOptions = () => ({
    login: document.getElementById("login").value,
})

const startAuthenticate = startAuthenticateOptions =>
    request("/authenticate/start", startAuthenticateOptions)

const getCredentials = options =>
    navigator.credentials.get({
        publicKey: makePublicKeyCredentialRequestOptions(options)
    })

const makePublicKeyCredentialRequestOptions = options => ({
    ...options,
    challenge: fromBase64(options.challenge),
    allowCredentials: options.allowCredentials.map(credentialId => ({
        type: "public-key",
        id: fromBase64(credentialId),
    }))
})

const makeAuthenticatorAssertionResponse = async credentials => ({
    challenge: fromBase64URL(getChallenge(credentials.response.clientDataJSON)),
    message: toBase64(concat(
        credentials.response.authenticatorData,
        await crypto.subtle.digest("SHA-256", credentials.response.clientDataJSON)
    )),
    signature: toBase64(credentials.response.signature),
})

const finishAuthenticate = finishAuthenticateOptions =>
    request("/authenticate/finish", finishAuthenticateOptions)

const getChallenge = (clientDataJSON) => {
    const decoder = new TextDecoder()
    return JSON.parse(decoder.decode(clientDataJSON)).challenge
}

const makePublicKeyCredentials = (credentials, response) => ({
    id: toBase64(credentials.rawId),
    response,
})

const request = async (url, data) => {
    const response = await fetch(url, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
        },
        body: JSON.stringify(data),
    })
    if (response.ok) {
        return response.json()
    }
    throw new Error(await response.text())
}

const fromBase64 = base64 =>
    Uint8Array.from(atob(base64), c => c.charCodeAt(0))

const toBase64 = data =>
    btoa(String.fromCharCode(...new Uint8Array(data)))

fromBase64URL = (data) => {
    const input = data
        .replace(/-/g, "+")
        .replace(/_/g, "/")
    const pad = input.length % 4
    if (pad) {
        if (pad === 1) {
            throw new Error("InvalidLengthError: Input base64url string is the wrong length to determine padding")
        }
        return input.padEnd(input.length + (4 - pad), "=")
    }
    return input
}

const concat = (a, b) => {
    const result = new Uint8Array(a.byteLength + b.byteLength)
    result.set(new Uint8Array(a))
    result.set(new Uint8Array(b), a.byteLength)
    console.log(a, b, result)
    console.log(a.length, b.length, result.length)
    return result
}

const debug = data => {
    const el = document.getElementById("debug")
    el.innerHTML =
        typeof data === "string"
            ? data
            : JSON.stringify(data, null, 4)
}

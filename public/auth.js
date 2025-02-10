const register = async () => {
    try {
        const registrationOptions = makeRegistrationOptions()
        console.log(registrationOptions)
        const publicKeyCredentialCreationOptions = await beginRegistration(registrationOptions)
        console.log(publicKeyCredentialCreationOptions)
        const credentials = await createCredentials(publicKeyCredentialCreationOptions)
        console.log(credentials)
        const attestationResponse = makeAuthenticatorAttestationResponse(credentials)
        console.log(attestationResponse)
        const publicKeyCredentials = makePublicKeyCredentials(credentials, attestationResponse)
        console.log(publicKeyCredentials)
        const res = await completeRegistration(publicKeyCredentials)
        console.log(res)
    } catch (err) {
        console.error(err.message)
        debug(err.message)
    }
}

const makeRegistrationOptions = () => ({
    login: document.getElementById("login").value,
    name: document.getElementById("name").value,
})

const beginRegistration = beginRegistrationOptions =>
    request("/registration/begin", beginRegistrationOptions)

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
    challenge: getChallenge(credentials.response.clientDataJSON),
    publicKey: toBase64(credentials.response.getPublicKey()),
    publicKeyAlgorithm: credentials.response.getPublicKeyAlgorithm(),
})

const completeRegistration = completeRequestOptions =>
    request("/registration/complete", completeRequestOptions)


const authenticate = async () => {
    try {
        const authenticationOptions = makeAuthenticationOptions()
        console.log(authenticationOptions)
        const publicKeyCredentialRequestOptions = await beginAuthentication(authenticationOptions)
        console.log(publicKeyCredentialRequestOptions)
        const credentials = await getCredentials(publicKeyCredentialRequestOptions)
        console.log(credentials)
        const assertionResponse = await makeAuthenticatorAssertionResponse(credentials)
        console.log(assertionResponse)
        const publicKeyCredentials = await makePublicKeyCredentials(credentials, assertionResponse)
        console.log(publicKeyCredentials)
        const res = await completeAuthentication(publicKeyCredentials)
        console.log(res)
    } catch (err) {
        console.error(err.message)
        debug(err.message)
    }
}

const makeAuthenticationOptions = () => ({
    login: document.getElementById("login").value,
})

const beginAuthentication = beginAuthenticationOptions =>
    request("/authentication/begin", beginAuthenticationOptions)

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
    challenge: getChallenge(credentials.response.clientDataJSON),
    message: toBase64(concat(
        credentials.response.authenticatorData,
        await crypto.subtle.digest("SHA-256", credentials.response.clientDataJSON)
    )),
    signature: toBase64(credentials.response.signature),
})

const completeAuthentication = completeAuthenticationOptions =>
    request("/authentication/complete", completeAuthenticationOptions)

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
